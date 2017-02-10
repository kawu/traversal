module NLP.Departage.CRF
(
) where


import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.MemoCombinators as Memo

import qualified NLP.Departage.Hype as H
import           NLP.Departage.Hype (Hype, Arc (..), Node (..))


------------------------------------------------------------------
-- Types
------------------------------------------------------------------


-- | A feature-based hypergraph, where each arc label is enriched with the set
-- of the corresponding features and their multiplicities.
--
-- NOTE: the multiplicities should be actually (positive) natural numbers, but
-- we do not enforce this (maybe it will be fun to use real numbers later on).
-- type FeatHype i j f = Hype i j (M.Map f Double)
-- type FeatHype i j f = Hype i j [f]


-- | CRF model: *exponential* values assigned to individual features.
type ExpCRF f v = f -> v


-- | A "potential" assigned by a model to a given element:
-- arc, feature, hyperpath, etc.
type Phi a v = a -> v


-- | Inside probabilities.
type Ins a v = a -> v


-- | Outside probabilities.
type Out a v = a -> v


-- | Probability of a given element.
type Prob a v = a -> v


-- | Features assigned to a given element, together with the
-- corresponding multiplicities.
--
-- TODO: we would like to allow fractional multiplicities, but then
-- `potential` requires that `v` is Floating and not just
-- Franctional.
-- type Feat a f v = a -> M.Map f v
type Feat a f = a -> M.Map f Int


-- -- | A class representing the numbers
-- class Num a where
--   -- | Add two numbers
--   add :: a -> a -> a
--
--   -- | Sum a list of numbers
--   sum :: [a] -> a
--   sum =


------------------------------------------------------------------
-- Basic functions
------------------------------------------------------------------


-- | A potential of a given arc.
potential :: Fractional v => ExpCRF f v -> Feat Arc f -> Arc -> v
potential crf featMap arc = product
  [ crf feat ^ occNum
  | (feat, occNum) <- M.toList (featMap arc) ]


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
marginals :: Fractional v => ExpCRF f v -> Hype -> Feat Arc f -> Prob Arc v
marginals crf hype feat =
  \arc -> insArc arc * outArc arc / zx
  where
    -- potential :: Floating v => ExpCRF f v -> Feat Arc f v -> Arc -> v
    phi = potential crf feat
    (insNode, insArc) = inside hype phi
    (outNode, outArc) = outside hype insNode insArc
    zx = normFactor hype insNode


-- | Expected number of occurrences of the individual features.
expected
  :: (Ord f, Num v)
  => Hype
  -> Prob Arc v   -- ^ Marginal arc probabilities
  -> Feat Arc f   -- ^ Set of features assigned to a given arc
  -> M.Map f v    -- ^ Expected number of occurrences of the individual features
expected hype probOn featMap = M.unionsWith (+) $ do
  i <- S.toList (H.nodes hype)
  j <- S.toList (H.ingoing i hype)
  let prob = probOn j
  return $ M.fromListWith (+)
    [ (feat, prob * fromIntegral occNum)
    | (feat, occNum) <- M.toList (featMap j) ]
