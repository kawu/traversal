{-# LANGUAGE DeriveFunctor #-}


-- | (Hyper)arc definition


module NLP.Departage.Parser.Arc
( Arc (..)
, Prio
, prioOn
, prio
) where


import           Data.Lens.Light

import qualified NLP.Departage.Parser.Item as I


-- | An arc represents an action of inducing a new item on the
-- basis of one or two other chart items.  It can be seen as an
-- application of one of the inference rules specifying the parsing
-- algorithm.
data Arc a b = Arc
  { left :: I.Item a
    -- ^ Left of the daughter items
  , right :: I.Item a
    -- ^ Right of the daughter items
  , west :: Bool
    -- ^ True if the arc is directed from `right` to `left`
  , label :: b
    -- ^ Label assigned to the arc
  } deriving (Show, Eq, Ord, Functor)


--------------------------------------------------
-- Priority
--------------------------------------------------


-- | Priority type.
type Prio = Int


-- | Priority of an item depending on its span. Makes sure that
-- states are removed from the queue in a specific order.
prioOn :: I.Span -> Prio
prioOn = getL I.end
-- prioOn :: Span -> Prio
-- prioOn s =
--   let i = getL beg s
--       j = getL end s
--   in  (j, j - i)


prio :: I.Item a -> Prio
prio = prioOn . getL I.span
