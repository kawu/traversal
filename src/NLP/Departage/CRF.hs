{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}


module NLP.Departage.CRF
(
-- * CRF
  inside
, outside
, normFactor
, marginals
, expected

-- * Probability
, probability

-- * Defaults
, defaultPotential
, defaultFeat

-- * Tests
, testHype
, testCRF
, testFeatBase
, testSGD
) where


import           Control.Monad (forM_, forM)
-- import qualified Control.Monad.ST as ST
-- import           Control.Monad.ST (ST)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.IO.Class (liftIO)
-- import qualified Control.Monad.Primitive as Prim
-- import qualified Control.Monad.State.Strict as State

-- import qualified Numeric.SGD.LogSigned as LogSigned
-- import qualified Numeric.SGD.Momentum as SGD

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.MemoCombinators as Memo
import qualified Data.Number.LogFloat as F
import qualified Data.PrimRef as Ref

import qualified Pipes as Pipes
-- import           Streaming.Prelude (Stream, Of)

import qualified NLP.Departage.Hype as H
import           NLP.Departage.Hype (Hype, Arc (..), Node (..))

import qualified NLP.Departage.CRF.SGD as SGD
import qualified NLP.Departage.CRF.SGD.Dataset as SGD.Dataset
import qualified NLP.Departage.CRF.Map as Map
import           NLP.Departage.CRF.Map
  (Flo(..), RefMap(..))
import           NLP.Departage.CRF.Mame (Mame)
import qualified NLP.Departage.CRF.Mame as Mame


------------------------------------------------------------------
-- Basic types
------------------------------------------------------------------


-- | Inside probabilities.
type Ins a v = a -> v


-- | Outside probabilities.
type Out a v = a -> v


-- | Probability of a given element.
type Prob a v = a -> v


-- | CRF model: *exponential* values assigned to individual features.
type ExpCRF f v = f -> v


-- | A "potential" assigned by a model to a given element: arc, feature,
-- hyperpath, etc.
type Phi a v = a -> v


-- | Computes the map (`map f v`) of features (`f`) assigned to a given element
-- `a` (e.g. `Arc`), together with the corresponding multiplicities (`v`), and
-- put them in the input map (note that computation is performed in the `prim`
-- monad). The function should not assume that the given map is empty.
type Feat a prim map f v = a -> map f v -> prim ()


-- | A version of `Feat` which creates the map and returns it.
type FeatBase a prim map f v = a -> prim (map f v)


------------------------------------------------------------------
-- CRF
------------------------------------------------------------------


-- -- | A composite CRF model, in which the way potentials are computed is
-- -- abstracted away. Parametrized by the implementation of the map (`m`), objects
-- -- used as features (`f`), and potential values (`v`).
-- data CRF m f v = CRF
--   { value :: ExpCRF f v
--     -- ^ The basic component of the model: values assigned to features
--   , feature :: Feat Arc m f v
--     -- ^ Features assigned to the individual arcs, together with their
--     -- multiplicities
--   , potential :: Phi Arc v
--     -- ^ For each arc, `potenial` should give a number between 0 (very
--     -- improbable arc) and positive infinitive (particularly plausible arc),
--     -- while value 1 is neutral.
--   }


------------------------------------------------------------------
-- Inference
------------------------------------------------------------------


-- | Compute the inside probabilities.
--
-- TODO: define in terms of generalized inside computation? (see `inside'`)
inside
  :: Num v
  => Hype                    -- ^ The underlying hypergraph
  -> Phi Arc v               -- ^ The potential of arcs
  -> (Ins Node v, Ins Arc v) -- ^ The resulting inside probabilities
inside hype phi =

  (insideNode, insideArc)

  where

    insideNode = Memo.wrap H.Node H.unNode Memo.integral insideNode'
    insideNode' i
      | S.null ingo = 1
      | otherwise = sum
          [ insideArc j
          | j <- S.toList ingo ]
      where ingo = H.incoming i hype

    insideArc = Memo.wrap H.Arc  H.unArc  Memo.integral insideArc'
    insideArc' j = phi j * product
      [ insideNode i
      | i <- S.toList (H.tail j hype) ]


-- | Compute the outside probabilities.
outside
  :: Fractional v
  => Hype                    -- ^ The underlying hypergraph
  -> Ins Node v              -- ^ Inside node probabilities
  -> Ins Arc v               -- ^ Inside arc probabilities
  -> (Out Node v, Out Arc v) -- ^ The resulting outside probabilities
outside hype insideNode insideArc =

  (outsideNode, outsideArc)

  where

    outsideNode = Memo.wrap H.Node H.unNode Memo.integral outsideNode'
    outsideNode' i
      | S.null outgo = 1
      | otherwise = ( sum
          [ outsideArc j * insideArc j
          | j <- S.toList outgo ]
          ) / insideNode i
      where outgo = H.outgoing i hype

    -- outsideArc = Memo.wrap H.Arc  H.unArc  Memo.integral outsideArc'
    outsideArc j = outsideNode (H.head j hype)


-- | Compute the normalization factor.
normFactor :: Num v => Hype -> Ins Node v -> v
normFactor hype ins = sum $ do
  i <- S.toList (H.final hype)
  return $ ins i


-- | Compute marginal probabilities of the individual arcs given the potential
-- function.
marginals :: Flo v => Phi Arc v -> Hype -> Prob Arc v
marginals phi hype =
  \arc -> insArc arc * outArc arc / zx
  where
    ( insNode, insArc) = inside hype phi
    (_outNode, outArc) = outside hype insNode insArc
    zx = normFactor hype insNode


-- | Expected number of occurrences of the individual features.
--
-- Note that it takes an existing map of expectations to update. We adopt this
-- strategy so that epxectation maps need not be created for each hypergraph
-- separately.
expected
  :: (Map.Map prim map f v)
  => Hype           -- ^ The underlying hypergraph
  -> Feat Arc prim map f v
                    -- ^ Feature function
  -> Prob Arc v     -- ^ Arc probabilities in the hypergraph
  -> map f v        -- ^ Existing map of expectations to update
  -> Mame map f v prim ()
expected hype feature probOn expMap =
  Mame.withValueBuffer $ \buffer -> lift $ do
    forM_ (S.toList $ H.arcs hype) $ \i -> do
      -- put features and their multiplicities in the buffer
      feature i buffer
      -- multiply the multiplicities by the probability of the arc
      let prob = probOn i
      Map.modify (*prob) buffer
      -- add the buffer to the result map
      Map.mergeWith (+) expMap buffer


-- expected
--   :: (Ord f, Flo v, Map m f v)
--   => CRF m f v      -- ^ The CRF model
--   -> Hype           -- ^ The underlying hypergraph
--   -> Prob Arc v     -- ^ Marginal arc probabilities in the hypergraph
--   -> m f v          -- ^ Expected number of occurrences of the individual features
-- expected crf hype probOn = unionsWith (+) $ do
--   i <- S.toList (H.nodes hype)
--   j <- S.toList (H.incoming i hype)
--   let prob = probOn j
--   return $ fromListWith (+)
--     [ (feat, prob * occNum)
--     | (feat, occNum) <- toList (feature crf j) ]


------------------------------------------------------------------
-- Training
------------------------------------------------------------------


-- | Training data element.
data Elem prim map f v = Elem
  { elemHype :: Hype
    -- ^ The hypergraph
  , elemFeat :: Feat Arc prim map f v
    -- ^ Function which assigns features `f` with their counts `v` to the
    -- individual arcs in `elemHype`; consider using `defaultFeat`
  , elemProb :: Prob Arc v
    -- ^ Prior probabilities of the individual arcs
  }


-- | Update the gradient (`map f Double`) with respect to the given dataset. To
-- each dataset element `Phi Arc v`, a function which gives the arc potentials
-- computed on the basis of the underlying `ExpCRF` model, is assigned (see also
-- `defaultPotential`).
--
-- Note that `gradOn` does not `clear` the input gradient.
gradOn
  :: ( Map.Map prim map f v
     , Map.Map prim map f Double
     )
  => [(Elem prim map f v, Phi Arc v)]
     -- ^ A list of dataset elements for which to compute the gradient
  -> map f Double
     -- ^ Gradient to update
  -> Mame map f v prim ()
gradOn elems grad = do
  -- positive component
  Mame.withValueBuffer $ \pos -> do
    -- negative component
    Mame.withValueBuffer $ \neg -> do
      -- first clear both buffers
      lift $ Map.clear pos >> Map.clear neg
      let localGrad = Grad {posGrad=pos, negGrad=neg}
      -- update the gradient w.r.t. each dataset element
      localGradOn elems localGrad
      -- update the global `Double`-based gradient
      lift $ mergeGradWith grad localGrad


-- | We split the gradient into the positive and negative components, for better
-- numerical stability. When doing computations in log-domain, there's actually
-- no other way (well, one could use use signed log-domain floats).
data Grad map f v = Grad
  { posGrad :: map f v
  , negGrad :: map f v
  }


-- | Update the gradient (`Grad`) with respect to the given dataset.
localGradOn
  :: (Map.Map prim map f v)
  => [(Elem prim map f v, Phi Arc v)]
     -- ^ A list of dataset elements for which to compute the gradient
  -> Grad map f v
     -- ^ The gradient to update
  -> Mame map f v prim ()
localGradOn elems Grad{..} = do
  -- update each dataset element
  forM_ elems $ \(Elem{..}, elemPhi) -> do
    -- the actual counts are computed based on priors
    expected elemHype elemFeat elemProb posGrad
    -- to compute expectation, we use posterior marginals instead
    let elemMarg = marginals elemPhi elemHype
    expected elemHype elemFeat elemMarg negGrad


-- | Update the normal domain gradient vector with the given (possibly
-- log-domain) `Grad`.
--
-- TODO: we separately add `posGrad` and substract `negGrad`. Alternatively, we
-- could try to first obtain one (possibly signed) gradient vector, and only
-- than add it to the gradient.
mergeGradWith
  :: ( Map.Map prim map f v
     , Map.Map prim map f Double
     )
  => map f Double
  -> Grad map f v
  -> prim ()
mergeGradWith paraMap Grad{..} = do
  let add param x = param + toDouble x
      sub param x = param - toDouble x
  Map.mergeWith add paraMap posGrad
  Map.mergeWith sub paraMap negGrad


-- gradOn :: Md.Model -> SGD.Para -> DAG a (X, Y) -> SGD.Grad
-- gradOn model para dag = SGD.fromLogList $
--     [ (Md.featToJustInt curr feat, L.fromPos val)
--     | (feat, val) <- featuresIn dag ] ++
--     [ (ix, L.fromNeg val)
--     | (Md.FeatIx ix, val) <- I.expectedFeaturesIn curr (fmap fst dag) ]
--   where
--     curr = model { Md.values = para }


------------------------------------------------------------------
-- Probability
------------------------------------------------------------------


-- | Generalized inside computation, where prior probabilities are assigned
-- to the individual arcs.
inside'
  :: Num v
  => Hype                    -- ^ The underlying hypergraph
  -> Phi Arc v               -- ^ The potential of arcs
  -> Prob Arc v              -- ^ The prior probabilities of arcs
  -> (Ins Node v, Ins Arc v) -- ^ The resulting inside probabilities
inside' hype phi prior =

  (insideNode, insideArc)

  where

    insideNode = Memo.wrap H.Node H.unNode Memo.integral insideNode'
    insideNode' i
      | S.null ingo = 1
      | otherwise = sum
          [ insideArc j
          | j <- S.toList ingo ]
      where ingo = H.incoming i hype

    insideArc = Memo.wrap H.Arc  H.unArc  Memo.integral insideArc'
    insideArc' j = prior j * phi j * product
      [ insideNode i
      | i <- S.toList (H.tail j hype) ]


-- | Probability of the element (graph) in the given model.
probability
  :: Flo v
  => Elem prim map f v
  -> Phi Arc v -- ^ Arc potentials, computed on the basis of `ExpCRF`
  -> v
probability Elem{..} phi =
  zx' / zx
  where
    zx' = normFactor elemHype . fst $ inside' elemHype phi elemProb
    zx =  normFactor elemHype . fst $ inside  elemHype phi


------------------------------------------------------------------
-- Defaults
------------------------------------------------------------------


-- | Computes the `potential`, given the `value` and `feature` components of a
-- CRF model. The function is correct only if parameters of the model do not
-- interact (for instance, they do not multiply together).
defaultPotential
  :: (Map.Map prim map f v)
  -- => Map.Dom f  -- ^ The domain of `ExpCRF f v`
  => ExpCRF f v
  -> Hype
  -> Feat Arc prim map f v
  -> Mame map f v prim (Phi Arc v)
defaultPotential crf hype feature = Mame.withValueBuffer $ \featMap -> do
  lift $ do
    fmap mkPhi . forM (S.toList $ H.arcs hype) $ \arc -> do
      acc <- Ref.newPrimRef 1
      Map.clear featMap >> feature arc featMap
      Pipes.runListT $ do
        (feat, occNum) <- Map.toList featMap
        let x = crf feat `power` occNum
        lift $ Ref.modifyPrimRef' acc (*x)
      res <- Ref.readPrimRef acc
      return (arc, res)
  where
    mkPhi xs =
      let m = M.fromList xs
      in  \arc -> case M.lookup arc m of
                    Just v  -> v
                    Nothing -> error "defaulPotential: incorrect arc"


-- defaultPotential
--   :: (Flo v, Map m f v)
--   => ExpCRF f v
--   -> Feat Arc m f v
--   -> Phi Arc v
-- defaultPotential crf feat arc = product
--   [ crf feat `power` occNum
--   | (feat, occNum) <- toList (feat arc) ]


-- | Create a default `Feat` implementation based on the corresponding `FeatBase`.
-- Of course it might not be optimal in certain situations.
defaultFeat
  :: (Map.Map prim map f v)
  => FeatBase a prim map f v
  -> Feat a prim map f v
defaultFeat featBase arc featMap = do
  Map.clear featMap
  featMap' <- featBase arc
  Map.mergeWith (\_ x -> x) featMap featMap'


------------------------------------------------------------------
-- Tests
------------------------------------------------------------------


testHype :: Hype
testHype = H.fromList
  [ ( Node 1, M.empty )
  , ( Node 2, M.empty )
  , ( Node 3, M.fromList
      [ (Arc 1, S.fromList [Node 1]) ]
    )
  , ( Node 4, M.fromList
      [ (Arc 2, S.fromList [Node 2, Node 3]) ]
    )
  , ( Node 5, M.empty )
  , ( Node 6, M.fromList
      [ (Arc 3, S.fromList [Node 2, Node 5]) ]
    )
  , ( Node 7, M.fromList
      [ (Arc 4, S.fromList [Node 3, Node 6]) ]
    )
  ]


testCRF :: ExpCRF String Double
testCRF x = case x of
  "on1" -> exp $  0.6931471805599453 -- 2
  "on2" -> exp $ -0.6931471805599453 -- 0.5
  "on3" -> exp $  0                  -- 1
  "on4" -> exp $  0.6931471805599453 -- 2
  _ ->     exp $  0                  -- 1


-- -- | Convert a given parameter map to `ExpCRF`.
-- --
-- -- TODO: Potentially quite inefficient!
-- crfFromParams
--   :: (Ord f, Map.Map prim map f Double)
--   => map f Double
--   -> prim (ExpCRF f Double)
-- crfFromParams paraMap = do
--   regMap <- flip State.runStateT M.empty $ do
--     Pipes.runListT $ do
--       -- now this is hoisting magic...
--       (para, val) <- Pipes.hoist lift $ Map.toList paraMap
--       lift . State.modify' $ M.insert para val
--   return $ \x -> exp $
--     case M.lookup x (snd regMap) of
--       Nothing -> 0
--       Just v  -> v


-- testFeatBase :: FeatBase Arc IO (RefMap IO) String Double
testFeatBase :: FeatBase Arc IO (RefMap IO) String F.LogFloat
testFeatBase (Arc x) = case x of
  1 -> mk1 "on1"
  2 -> mk1 "on2"
  3 -> mk1 "on3"
  4 -> mk1 "on4"
  _ -> Map.newRefMap M.empty
  where
    mk1 f = Map.newRefMap $ M.singleton f 1


-- testAll :: IO ()
-- testAll = do
--   -- map for expected elements
--   ex <- Map.newRefMap M.empty
--   let testFeat = defaultFeat testFeatBase
--   runMame (Map.newRefMap M.empty) $ do
--     phi <- defaultPotential testValue testHype testFeat
--     let marg = marginals phi testHype
--     expected testHype (defaultFeat testFeatBase) marg ex
--   print =<< Ref.readPrimRef (unRefMap ex)


-- testGrad :: IO ()
-- testGrad = do
--   -- gradient map
--   grad <- Map.newRefMap M.empty
--   let testFeat = defaultFeat testFeatBase
--   -- Mame layer for parameter vectors
--   runMame (Map.newRefMap M.empty) $ do
--     phi <- defaultPotential testValue testHype testFeat
--     let dataElem =
--           ( Elem
--             { elemHype = testHype
--             , elemFeat = testFeat
--             -- below, the target probabilities
--             , elemProb = \arc -> case arc of
--                 Arc 1 -> 1.0
--                 Arc 2 -> 0.0
--                 Arc 3 -> 1.0
--                 Arc 4 -> 1.0
--                 Arc _ -> error "no such arc"
--             }
--           , phi )
--     liftIO . print $ uncurry probability dataElem
--     gradOn [dataElem] grad
--     liftIO $ print =<< Ref.readPrimRef (unRefMap grad)


testSGD :: IO ()
testSGD = do

  -- parameter map
  paraMap <- Map.newRefMap M.empty

  -- enrich the dataset elements with potential functions, based on the current
  -- parameter values (and using `defaultPotential`)
  let withPhi xs = do
        paraPure <- liftIO $ Map.unsafeFreeze paraMap
        let crf x = F.logToLogFloat $ case paraPure x of
              Nothing -> 0
              Just v  -> v
        forM xs $ \x@Elem{..} -> do
          phi <- defaultPotential crf elemHype elemFeat
          return (x, phi)

  -- notification function
  let notify k
        | doneTotal k == doneTotal (k - 1) =
            liftIO $ putStr "."
        | otherwise = do
            -- parameters
            liftIO $ do
              putStrLn "" >> putStr "params: "
              print =<< Ref.readPrimRef (unRefMap paraMap)
            -- probability
            liftIO $ putStr "probability: "
            batch <- withPhi dataList
            liftIO . print . product $ map (uncurry probability) batch

  -- gradient computation
  let computeGradient grad batch0 = do
        batch <- withPhi batch0
        gradOn batch grad

  -- SGD parameters
  let sgdParams = SGD.sgdArgsDefault
        { SGD.batchSize = batchSize
        , SGD.iterNum = 10
        , SGD.regVar = 4
        , SGD.gain0 = 0.25
        , SGD.tau = 5
        , SGD.gamma = 0.9
        }

  -- Buffer creation
  let makeBuff = Mame.Make
        { Mame.mkValueBuffer = Map.newRefMap M.empty
        , Mame.mkDoubleBuffer = Map.newRefMap M.empty
        }

  -- Convert the dataset
  SGD.Dataset.withVect dataList $ \dataSet -> do
    -- TODO: change the type of `sgd` so that it runs internally `runMame` with
    -- provided `makeBuff`
    Mame.runMame makeBuff $ do
      SGD.sgd
        sgdParams
        notify
        computeGradient
        dataSet
        paraMap

  where

    -- dataset
    dataList = replicate 1000 $ Elem
      { elemHype = testHype
      , elemFeat = defaultFeat testFeatBase
      -- below, the target probabilities
      , elemProb = \arc -> case arc of
          Arc 1 -> 1.0
          Arc 2 -> 0.0
          Arc 3 -> 1.0
          Arc 4 -> 1.0
          Arc _ -> error "no such arc"
      }

    batchSize = 30
    trainSize = length dataList

    doneTotal :: Int -> Int
    doneTotal = floor . done

    done :: Int -> Double
    done i
        = fromIntegral (i * batchSize)
        / fromIntegral trainSize
