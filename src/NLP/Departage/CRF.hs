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
, testValue
, testFeatBase
, testAll
, testGrad
) where


import           Control.Monad (forM_, forM)
-- import qualified Control.Monad.ST as ST
-- import           Control.Monad.ST (ST)
import           Control.Monad.Trans.Class (lift)
-- import qualified Control.Monad.Primitive as Prim

-- import qualified Numeric.SGD.LogSigned as LogSigned
-- import qualified Numeric.SGD.Momentum as SGD

import qualified Data.PrimRef as Ref

import qualified Pipes as Pipes
-- import           Streaming.Prelude (Stream, Of)

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.MemoCombinators as Memo

import qualified NLP.Departage.Hype as H
import           NLP.Departage.Hype (Hype, Arc (..), Node (..))
import           NLP.Departage.CRF.Map
import           NLP.Departage.CRF.Mame


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
  :: (Map prim map f v)
  => Hype           -- ^ The underlying hypergraph
  -> Feat Arc prim map f v
                    -- ^ Feature function
  -> Prob Arc v     -- ^ Arc probabilities in the hypergraph
  -> map f v        -- ^ Existing map of expectations to update
  -> Mame (map f v) prim ()
expected hype feature probOn expMap =
  withBuffer $ \buffer -> lift $ do
    forM_ (S.toList $ H.arcs hype) $ \i -> do
      -- put features and their multiplicities in the buffer
      feature i buffer
      -- multiply the multiplicities by the probability of the arc
      let prob = probOn i
      modify (*prob) buffer
      -- add the buffer to the result map
      mergeWith (+) expMap buffer


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


-- Random thoughts.
--
-- The question is now how the "global paramter map" should be represented. SGD
-- library assumes that it's an unboxed `Double` vector. The advantage of this
-- assumption over, e.g., using regular maps is that we need to scale the entire
-- map with each SGD iteration. Obviously it works faster with vectors.
--
-- But... this way we enforce that features are ints or, at least, that there is
-- a bijection between features and integers. So if we want to allow using this
-- code with any `Ord f`, using vectors doesn't sound like a terribly good idea.
--
-- Besides, not assuming much about the form of the global map does not mean
-- that we cannot use vectors and that the implementation cannot be efficient.
-- So it would be better to leave this underspecified.
--
--
--
-- Now, what should we assume about the parameter values? Because we plan to use
-- log-floats, we cannot just use `v` (signed log-floats could be a solution,
-- but the link from the logfloat package doesn't work, so it's not clear if it
-- is available somewhere; besides, it would be a lot of computational overhead
-- for not that much).
--
-- Using a different parameter, e.g. `w`, could be a solution, but is it worth
-- it?  Well, for our current purposes, not really, `Double`s are enough. And if
-- we need to generalize it further later on, then we will do that later on...


-- | Training data element.
data Elem prim map f v = Elem
  { elemHype :: Hype
    -- ^ The hypergraph
  , elemFeat :: Feat Arc prim map f v
    -- ^ Function which assigns features `f` with their counts `v` to the
    -- individual arcs in `elemHype`; consider using `defaultFeat`
  , elemProb :: Prob Arc v
    -- ^ Prior probabilities of the individual arcs
  , elemPhi  :: Phi Arc v
    -- ^ Arc potentials computed on the basis of the underlying `ExpCRF` model;
    -- consider using `defaultPotential`
  }


-- | Update the gradient (`map f Double`) with respect to the given dataset. The
-- function does not `clear` the input gradient.
gradOn
  :: (Ord f, Map prim map f v, Map prim map f Double)
  => [Elem prim map f v]
     -- ^ A list of dataset elements for which to compute the gradient
  -> map f Double
     -- ^ Gradient to update
  -> Mame (map f v) prim ()
gradOn elems grad = do
  -- positive component
  withBuffer $ \pos -> do
    -- negative component
    withBuffer $ \neg -> do
      -- first clear both buffers
      lift $ clear pos >> clear neg
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
  :: (Ord f, Map prim map f v)
  => [Elem prim map f v]
     -- ^ A list of dataset elements for which to compute the gradient
  -> Grad map f v
     -- ^ The gradient to update
  -> Mame (map f v) prim ()
localGradOn elems Grad{..} = do
  -- update each dataset element
  forM_ elems $ \Elem{..} -> do
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
  :: ( Map prim map f v
     , Map prim map f Double
     )
  => map f Double
  -> Grad map f v
  -> prim ()
mergeGradWith paraMap Grad{..} = do
  let add param x = param + toDouble x
      sub param x = param - toDouble x
  mergeWith add paraMap posGrad
  mergeWith sub paraMap negGrad


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
probability :: Flo v => Elem prim map f v -> v
probability Elem{..} =
  zx' / zx
  where
    zx' = normFactor elemHype . fst $ inside' elemHype elemPhi elemProb
    zx =  normFactor elemHype . fst $ inside  elemHype elemPhi


------------------------------------------------------------------
-- Defaults
------------------------------------------------------------------


-- | Computes the `potential`, given the `value` and `feature` components of a
-- CRF model. The function is correct only if parameters of the model do not
-- interact (for instance, they do not multiply together).
--
-- NOTE: the function will not be so straightforward once the feature computing
-- function is embedded in a monad. Then we will probably need, as input
-- argument, the entire hypergraph, so that `defaultPotential` can return
-- something like `m (Phi Arc v)`.
defaultPotential
  :: (EnumerableMap prim map f v)
  => ExpCRF f v
  -> Hype
  -> FeatBase Arc prim map f v
  -> prim (Phi Arc v)
defaultPotential crf hype feature =
  fmap mkPhi . forM (S.toList $ H.arcs hype) $ \arc -> do
    acc <- Ref.newPrimRef 1
    featMap <- feature arc
    Pipes.runListT $ do
      (feat, occNum) <- toList featMap
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
  :: (Map prim map f v)
  => FeatBase a prim map f v
  -> Feat a prim map f v
defaultFeat featBase arc featMap = do
  clear featMap
  featMap' <- featBase arc
  mergeWith (\_ x -> x) featMap featMap'


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


testValue :: ExpCRF String Double
testValue x = case x of
  "on1" -> exp $  0.6931471805599453 -- 2
  "on2" -> exp $ -0.6931471805599453 -- 0.5
  "on3" -> exp $  0                  -- 1
  "on4" -> exp $  0.6931471805599453 -- 2
  _ -> 1


-- testValue2 :: ExpCRF String Double
-- testValue2 x = case x of
--   "on1" -> exp $  0.6931471805599453
--   "on2" -> exp $ -0.6931471805599453 + 0.3 + 0.11923309082120659 + 3.2104542429114646e-2
--   "on3" -> exp $  0                  - 0.3 - 0.11923309082120659 - 3.2104542429114646e-2
--   "on4" -> exp $  0.6931471805599453 - 0.3 - 0.11923309082120659 - 3.2104542429114646e-2
--   _ -> 1


testFeatBase :: FeatBase Arc IO (RefMap IO) String Double
testFeatBase (Arc x) = case x of
  1 -> mk1 "on1"
  2 -> mk1 "on2"
  3 -> mk1 "on3"
  4 -> mk1 "on4"
  _ -> newRefMap M.empty
  where
    mk1 f = newRefMap $ M.singleton f 1


testAll :: IO ()
testAll = do
  phi <- defaultPotential testValue testHype testFeatBase
  let marg = marginals phi testHype
  ex <- newRefMap M.empty
  runMame (newRefMap M.empty) $ do
    expected testHype (defaultFeat testFeatBase) marg ex
  print =<< Ref.readPrimRef (unRefMap ex)


testGrad :: IO ()
testGrad = do
  phi <- defaultPotential testValue testHype testFeatBase
  let dataElem = Elem
        { elemHype = testHype
        , elemFeat = defaultFeat testFeatBase
        -- below, the target probabilities
        , elemProb = \arc -> case arc of
            Arc 1 -> 1.0
            Arc 2 -> 0.0
            Arc 3 -> 1.0
            Arc 4 -> 1.0
            Arc _ -> error "no such arc"
        , elemPhi = phi
        }
  print $ probability dataElem
  grad <- newRefMap M.empty
  runMame (newRefMap M.empty) $ do
    gradOn [dataElem] grad
  print =<< Ref.readPrimRef (unRefMap grad)
