{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}


module NLP.Departage.CRF
(
-- * Maps
  Map(..)
, RefMap(..)
, newRefMap

-- * CRF
, inside
, outside
, normFactor
, marginals
, ExpMaps(..)
, expected

-- * Defaults
, defaultPotential
, defaultFeat

-- * Tests
, testHype
, testValue
, testFeatBase
, testAll
) where


import           Control.Monad (forM_, forM)
-- import qualified Control.Monad.ST as ST
-- import           Control.Monad.ST (ST)
import           Control.Monad.Trans.Class (lift)
-- import qualified Control.Monad.Primitive as Prim
import           Control.Monad.Primitive (PrimMonad)

import qualified Data.PrimRef as Ref

import qualified Pipes as Pipes
-- import           Streaming.Prelude (Stream, Of)

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.MemoCombinators as Memo
import qualified Data.Number.LogFloat as LogFloat

import qualified NLP.Departage.Hype as H
import           NLP.Departage.Hype (Hype, Arc (..), Node (..))


------------------------------------------------------------------
-- Flo(ating)
------------------------------------------------------------------


-- | A class representing floating point numbers, limited to operations which we
-- rely on.
class Fractional a => Flo a where
  power :: a -> a -> a

instance Flo Double where
  power = (**)

instance Flo LogFloat.LogFloat where
  power x = LogFloat.pow x . LogFloat.fromLogFloat


------------------------------------------------------------------
-- Map
------------------------------------------------------------------


-- -- | A class-based representation of maps, so we can plug different
-- -- implementations (regular maps, hash-based maps, arrays, etc.).
-- class Map m k v where
--   toList :: m k v -> [(k, v)]
--   fromListWith :: (v -> v -> v) -> [(k, v)] -> m k v
--   unionsWith :: (v -> v -> v) -> [m k v] -> m k v
--
-- instance Ord k => Map M.Map k v where
--   toList = M.toList
--   fromListWith = M.fromListWith
--   unionsWith = M.unionsWith


-- | A class-based representation of maps, so we can plug different
-- implementations (regular maps, hash-based maps, arrays, etc.).
--
-- TODO: describe the minimal implementation.
class Map prim map k v where

  -- | Apply the function to all elements in the map
  modify :: (v -> v) -> map k v -> prim ()

  -- | Union of two maps with the given merging function. The result should be
  -- stored in the first map.
  unionWith :: (v -> v -> v) -> map k v -> map k v -> prim ()

  -- | The stream of keys in the map
  toList :: map k v -> Pipes.ListT prim (k, v)
  -- keys :: map k v -> Stream (Of k) prim ()

  -- | Set all elements in the map to `0`
  clear :: Num v => map k v -> prim ()
  clear = modify $ const 0

  -- toList :: map k v -> prim [(k, v)]
  -- fromListWith :: (v -> v -> v) -> [(k, v)] -> prim (map k v)
  -- unionsWith :: (v -> v -> v) -> [map k v] -> prim (map k v)


-- | Referentece to a `M.Map`. We rely on a convention that the value assigned
-- to the elements not in the map is 0.
newtype RefMap prim k v =
  RefMap {unRefMap :: Ref.PrimRef prim (M.Map k v)}


-- | Create a `RefMap` with a given map.
newRefMap :: PrimMonad m => M.Map k v -> m (RefMap m k v)
newRefMap m = do
  ref <- Ref.newPrimRef m
  return $ RefMap ref


instance (PrimMonad prim, Ord k) => Map prim (RefMap prim) k v where
  modify f RefMap{..} = do
    m <- Ref.readPrimRef unRefMap
    Ref.writePrimRef unRefMap (fmap f m)
  unionWith f ref1 ref2 = do
    m1 <- Ref.readPrimRef $ unRefMap ref1
    m2 <- Ref.readPrimRef $ unRefMap ref2
    Ref.writePrimRef (unRefMap ref1) (M.unionWith f m1 m2)
  toList RefMap{..} = do
    m <- lift $ Ref.readPrimRef unRefMap
    Pipes.Select . Pipes.each $ M.toList m
  clear RefMap{..} = Ref.writePrimRef unRefMap M.empty


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


-- | Computes the map (`map f v`) of features (`f`) assigned to a given elements
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
-- Basic functions
------------------------------------------------------------------


-- | Compute the inside probabilities.
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


-- | Maps given on input to the `expected` function.
data ExpMaps map f v = ExpMaps
  { expMap :: map f v
    -- ^ Expected number of occurrences of the individual features
  , buffer :: map f v
    -- ^ A buffer which can be used by `expected` for internal use
  }


-- | Expected number of occurrences of the individual features.
--
-- 1) The resulting map (param p -> E[p]) can be stored in an unboxed vector,
--    provided on input and gradually updated by the `expected` function. This
--    solution is especially desirable when the number of parameters is high
--    already at the level of the individual arcs (we will call such arcs
--    "dense") -- creating a separate map for each arc would be significantly
--    slower in this case. It should also work sufficiently well for sparse
--    arcs, since then we can reuse a single (or a couple of, if we want to
--    parallelize) array for the entire (sub-)batch.
--
-- 2) For each arc, the `feature` function should provide the respective
--    features together with their corresponding multiplicities. The function
--    could take an empty array of parameter values as input, fill it and
--    return. This would be a good strategy for dense arcs, but not so good for
--    sparse arcs. Using maps (potentially hash-based maps) would be good for
--    sparse arcs but not for dense arcs. Streaming could be a reasonable
--    middle-ground solution, but it could be less precise (a stream could,
--    depending on the model, return one and the same feature several times).
--
-- 3) We could abstract from the implementation of maps, as we were trying to
--    do, and support both regular maps and unboxed vectors.
--
-- 4) Another question is: do we need mutable vectors, or could/shoud we stick
--    to an immutable solution? For simplicity, let's assume that we are working
--    with unboxed vectors and not maps (or both). It seems clear that the
--    top-level vector (cf. 1) should be mutable. Well, it doesn't have to
--    mutable when given to the `expected` function as argument, but it will be
--    most likely more convenient to `unsafeThaw` it once, traverse all the arcs
--    of the hypergraph, update the mutable top-level vector, and `unsafeFreeze`
--    it in the end. Maybe we could do similarly with the `feature` function --
--    it could take an immutable vector, thaw it, play with it, freeze it and
--    return. But then, do we gain anything by doing that? It seems not. It has
--    to be specified in the description of the `feature` function that its
--    input argument cannot be used after the function is evaluated (cf.
--    `unsafeThaw`).
--
-- %) TO CONCLUDE: it seems that the best solution will be to (a) use mutable
--    vectors for both the top-level and the `feature` vectors, and (b) try to
--    abstract from the actual implementation of the map/vector.
--
-- $) COMMENT: Ok, so the `feature` function will be embeded in a monad. How are
--    we going to compute the potentials of the individual arcs (cf.
--    `defaultPotential`)? Well, as noted in a comment to `defaultPotential`
--    itself, we will have to traverse the entire hypergraph to compute its
--    results, store it in something like a `M.Map Arc v`, and return something
--    like `m (Phi Arc v)`.
--
-- $) COMMENT CD.: Great, so now `inside`, `outside`, `normFactor`, and
--    `marginals` are fine as they are, no need to update them to account for
--    mutable maps. Only the `expected` function (and `defaultPotential`) must
--    be updated.
expected
  :: (Ord f, Flo v, PrimMonad prim, Map prim map f v)
  => Feat Arc prim map f v
                    -- ^ The feature function
  -> Hype           -- ^ The underlying hypergraph
  -> Prob Arc v     -- ^ Marginal arc probabilities in the hypergraph
  -> ExpMaps map f v
  -> prim ()
expected feature hype probOn ExpMaps{..} =
  forM_ (S.toList $ H.arcs hype) $ \i -> do
    -- put features and their multiplicities in the buffer
    feature i buffer
    -- multiply the multiplicities by the probability of the arc
    let prob = probOn i
    modify (*prob) buffer
    -- add the buffer to the result map
    unionWith (+) expMap buffer

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
  :: (Flo v, PrimMonad prim, Map prim map f v)
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
  :: (Num v, PrimMonad prim, Map prim map f v)
  => FeatBase a prim map f v
  -> Feat a prim map f v
defaultFeat featBase arc featMap = do
  clear featMap
  featMap' <- featBase arc
  unionWith (\_ x -> x) featMap featMap'



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
  "on1" -> 2
  "on2" -> 0.5
  "on3" -> 1
  "on4" -> 2
  _ -> 1


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
  buf <- newRefMap M.empty
  expected (defaultFeat testFeatBase) testHype marg $ ExpMaps
    { expMap = ex
    , buffer = buf }
  print =<< Ref.readPrimRef (unRefMap ex)
