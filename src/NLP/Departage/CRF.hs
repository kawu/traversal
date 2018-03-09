{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module NLP.Departage.CRF
(
  inside
, outside
, normFactor
, marginals
, expected

-- * Tests
, testHype
, testValue
, testFeat
, testCRF
) where


import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.MemoCombinators as Memo
import qualified Data.Number.LogFloat as LogFloat

import qualified NLP.Departage.Hype as H
import           NLP.Departage.Hype (Hype, Arc (..), Node (..))


------------------------------------------------------------------
-- Classes
------------------------------------------------------------------


-- | A class representing floating point numbers, limited to operations which we
-- rely on.
class Fractional a => Flo a where
  power :: a -> a -> a

instance Flo Double where
  power = (**)

instance Flo LogFloat.LogFloat where
  power x = LogFloat.pow x . LogFloat.fromLogFloat


-- | A class-based representation of maps, so we can plug different
-- implementations (regular maps, hash-based maps, arrays, etc.).
class Map m k v where
  toList :: m k v -> [(k, v)]
  fromListWith :: (v -> v -> v) -> [(k, v)] -> m k v
  unionsWith :: (v -> v -> v) -> [m k v] -> m k v

instance Ord k => Map M.Map k v where
  toList = M.toList
  fromListWith = M.fromListWith
  unionsWith = M.unionsWith


------------------------------------------------------------------
-- Types
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


-- | Features assigned to a given element, together with the corresponding
-- multiplicities.
type Feat a m f v = a -> m f v


-- | A composite CRF model, in which the way potentials are computed is
-- abstracted away. Parametrized by the implementation of the map (`m`), objects
-- used as features (`v`), and potential values (`v`).
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


-- | Computes the `potential`, given the `value` and `feature` components of a
-- CRF model. The function is correct only if parameters of the model do not
-- interact (for instance, they do not multiply together).
defaultPotential
  :: (Flo v, Map m f v)
  => ExpCRF f v
  -> Feat Arc m f v
  -> Phi Arc v
defaultPotential crf featMap arc = product
  [ crf feat `power` occNum
  | (feat, occNum) <- toList (featMap arc) ]


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
  -> Ins Arc v               -- ^ Inside node probabilities
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
marginals
  :: (Flo v, Map m f Double)
  => CRF m f v
  -> Hype
  -> Prob Arc v
marginals crf hype =
  \arc -> insArc arc * outArc arc / zx
  where
    phi = potential crf
    ( insNode, insArc) = inside hype phi
    (_outNode, outArc) = outside hype insNode insArc
    zx = normFactor hype insNode


-- | Expected number of occurrences of the individual features.
expected
  :: (Ord f, Flo v, Map m f v)
  => Hype
  -> Prob Arc v     -- ^ Marginal arc probabilities
  -> Feat Arc m f v -- ^ Set of features assigned to a given arc
  -> m f v          -- ^ Expected number of occurrences of the individual features
expected hype probOn featMap = unionsWith (+) $ do
  i <- S.toList (H.nodes hype)
  j <- S.toList (H.ingoing i hype)
  let prob = probOn j
  return $ fromListWith (+)
    [ (feat, prob * occNum)
    | (feat, occNum) <- toList (featMap j) ]


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


testFeat :: Feat Arc M.Map String Double
testFeat (Arc x) = case x of
  1 -> mk1 "on1"
  2 -> mk1 "on2"
  3 -> mk1 "on3"
  4 -> mk1 "on4"
  _ -> M.empty
  where mk1 f = M.singleton f 1


testCRF :: CRF M.Map String Double
testCRF = CRF
  { value = testValue
  , feature = testFeat
  , potential = defaultPotential testValue testFeat
  }
