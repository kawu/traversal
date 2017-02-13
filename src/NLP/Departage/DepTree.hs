{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}


-- | Module for working with dependency trees.


module NLP.Departage.DepTree
(
-- * Types
  Tree (..)

-- * Functions
, mapN

-- * Conversion
, toRose

-- * Utils
, discard
) where


import qualified Control.Arrow as Arr
import qualified Data.Tree as R


---------------------------------------------------
-- Dependency tree
---------------------------------------------------


-- | A dependency tree with node (token) labels of type `a` and
-- arc labels of type `b`.
data Tree a b = Tree
  { root     :: a
    -- ^ Label assigned to the root of a dependency tree
  , children :: [(Tree a b, b)]
    -- ^ Children dependency trees and the corresponding dependency labels
  } deriving (Show, Eq, Ord, Functor)


-- | Map a function over values assigned to tree nodes.
mapN :: (a -> c) -> Tree a b -> Tree c b
mapN f Tree{..} = Tree
  (f root)
  (map (Arr.first (mapN f)) children)


---------------------------------------------------
-- Conversion
---------------------------------------------------


-- | Transform the dependency tree to a rose tree.
toRose :: Tree a b -> R.Tree (a, Maybe b)
toRose =
  go Nothing
  where
    go rootArc Tree{..} =
      R.Node
      { R.rootLabel = (root, rootArc)
      , R.subForest =
        [ go (Just arc) child
        | (child, arc) <- children ] }


---------------------------------------------------
-- Utils
---------------------------------------------------


-- | Discard dependency nodes which satisfy the given predicate.
-- Return `Nothing` when all nodes are discarded.
discard :: (a -> Bool) -> Tree a b -> [Tree a b]
discard p tree
  | p (root tree) = map fst newChildren
  | otherwise = [Tree (root tree) newChildren]
  where
    newChildren =
      [ (newChild, dep)
      | (oldChild, dep) <- children tree
      , newChild <- discard p oldChild ]
