module NLP.Departage.CRF
(
) where


import qualified Data.Set as S
import qualified Data.Map.Strict as M

import qualified NLP.Departage.Hype as H


------------------------------------------------------------------
-- Types
------------------------------------------------------------------


-- | A feature-based hypergraph, where each arc label is enriched with the set
-- of the corresponding features and their multiplicities.
--
-- NOTE: the multiplicities should be actually (positive) natural numbers, but
-- we do not enforce this (maybe it will be fun to use real numbers later on).
-- type FeatHype i j f = H.Hype i j (M.Map f Double)
type FeatHype i j f = H.Hype i j [f]


-- | A CRF model is simply a function which assignes values of type `v` to
-- features of type `f`.
type CRF f v = f -> v


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
phi :: Num v => CRF f v -> j -> FeatHype i j f -> v
phi crf j h = product [crf x | x <- H.label j h]


-- | Inside value for a given node.
insideNode :: Num v => CRF f v -> i -> FeatHype i j f -> v
insideNode crf i h = sum
  [ insideArc crf j h
  | j <- S.toList (H.ingoing i h) ]


-- | Inside value for a given arc.
insideArc :: Num v => CRF f v -> j -> FeatHype i j f -> v
insideArc crf j h = phi crf j h * product
  [ insideNode crf i h
  | i <- S.toList (H.tail j h) ]
