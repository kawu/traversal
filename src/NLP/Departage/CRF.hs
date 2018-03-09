{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module NLP.Departage.CRF
(
--   CRF (..)
-- , inside
-- , outside
-- , normFactor
-- , marginals
-- , expected
--
-- -- * Tests
-- , testHype
-- , testValue
-- , testFeat
-- , testCRF
) where


import qualified Control.Monad.ST as ST
import           Control.Monad.ST (ST)

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
class Map m k v where
  toList :: m k v -> [(k, v)]
  fromListWith :: (v -> v -> v) -> [(k, v)] -> m k v
  unionsWith :: (v -> v -> v) -> [m k v] -> m k v


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


-- | Computes the features assigned to a given element, together with the
-- corresponding multiplicities. A feature function can assume that the input
-- map (`m f v`) is empty.
--
-- TODO: We could use `ST s` instead of `IO`, but then the `CRF` type gest ugly.
-- Maybe the `CRF` type should be split itself?
type Feat a m f v = a -> m f v -> IO (m f v)


------------------------------------------------------------------
-- CRF
------------------------------------------------------------------


-- | A composite CRF model, in which the way potentials are computed is
-- abstracted away. Parametrized by the implementation of the map (`m`), objects
-- used as features (`f`), and potential values (`v`).
--
-- TODO: It seems that a CRF does not have much sense without its underlying
-- hypergraph. So maybe it should be included in the data structure?
data CRF m f v = CRF
  { value :: ExpCRF f v
    -- ^ The basic component of the model: values assigned to features
  , feature :: Feat Arc m f v
    -- ^ Features assigned to the individual arcs, together with their
    -- multiplicities
  , potential :: Phi Arc v
    -- ^ For each arc, `potenial` should give a number between 0 (very
    -- improbable arc) and positive infinitive (particularly plausible arc),
    -- while value 1 is neutral.
  }


------------------------------------------------------------------
-- Basic functions
------------------------------------------------------------------


-- -- | Computes the `potential`, given the `value` and `feature` components of a
-- -- CRF model. The function is correct only if parameters of the model do not
-- -- interact (for instance, they do not multiply together).
-- --
-- -- NOTE: the function will not be so straightforward once the feature computing
-- -- function is embedded in a monad. Then we will probably need, as input
-- -- argument, the entire hypergraph, so that `defaultPotential` can return
-- -- something like `m (Phi Arc v)`.
-- defaultPotential
--   :: (Flo v, Map m f v)
--   => ExpCRF f v
--   -> Feat Arc m f v
--   -> Phi Arc v
-- defaultPotential crf feat arc = product
--   [ crf feat `power` occNum
--   | (feat, occNum) <- toList (feat arc) ]


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
      where ingo = H.ingoing i hype

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


-- | Compute marginal probabilities of the individual arcs.
marginals :: Flo v => CRF m f v -> Hype -> Prob Arc v
marginals crf hype =
  \arc -> insArc arc * outArc arc / zx
  where
    phi = potential crf
    ( insNode, insArc) = inside hype phi
    (_outNode, outArc) = outside hype insNode insArc
    zx = normFactor hype insNode


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
--
-- expected
--   :: (Ord f, Flo v, Map m f v)
--   => CRF m f v      -- ^ The CRF model
--   -> Hype           -- ^ The underlying hypergraph
--   -> Prob Arc v     -- ^ Marginal arc probabilities in the hypergraph
--   -> m f v          -- ^ Expected number of occurrences of the individual features
-- expected crf hype probOn = unionsWith (+) $ do
--   i <- S.toList (H.nodes hype)
--   j <- S.toList (H.ingoing i hype)
--   let prob = probOn j
--   return $ fromListWith (+)
--     [ (feat, prob * occNum)
--     | (feat, occNum) <- toList (feature crf j) ]
--
-- expected
--   :: (Ord f, Flo v, Map m f v)
--   => CRF m f v      -- ^ The CRF model
--   -> Hype           -- ^ The underlying hypergraph
--   -> Prob Arc v     -- ^ Marginal arc probabilities in the hypergraph
--   -> m f v          -- ^ Expected number of occurrences of the individual features
-- expected crf hype probOn = unionsWith (+) $ do
--   i <- S.toList (H.nodes hype)
--   j <- S.toList (H.ingoing i hype)
--   let prob = probOn j
--   return $ fromListWith (+)
--     [ (feat, prob * occNum)
--     | (feat, occNum) <- toList (feature crf j) ]


------------------------------------------------------------------
-- Tests
------------------------------------------------------------------


-- testHype :: Hype
-- testHype = H.fromList
--   [ ( Node 1, M.empty )
--   , ( Node 2, M.empty )
--   , ( Node 3, M.fromList
--       [ (Arc 1, S.fromList [Node 1]) ]
--     )
--   , ( Node 4, M.fromList
--       [ (Arc 2, S.fromList [Node 2, Node 3]) ]
--     )
--   , ( Node 5, M.empty )
--   , ( Node 6, M.fromList
--       [ (Arc 3, S.fromList [Node 2, Node 5]) ]
--     )
--   , ( Node 7, M.fromList
--       [ (Arc 4, S.fromList [Node 3, Node 6]) ]
--     )
--   ]
--
--
-- testValue :: ExpCRF String Double
-- testValue x = case x of
--   "on1" -> 2
--   "on2" -> 0.5
--   "on3" -> 1
--   "on4" -> 2
--   _ -> 1
--
--
-- testFeat :: Feat Arc M.Map String Double
-- testFeat (Arc x) = case x of
--   1 -> mk1 "on1"
--   2 -> mk1 "on2"
--   3 -> mk1 "on3"
--   4 -> mk1 "on4"
--   _ -> M.empty
--   where mk1 f = M.singleton f 1
--
--
-- testCRF :: CRF M.Map String Double
-- testCRF = CRF
--   { value = testValue
--   , feature = testFeat
--   , potential = defaultPotential testValue testFeat
--   }
