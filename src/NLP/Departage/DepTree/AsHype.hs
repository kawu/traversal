{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}


-- | Transforming dependency trees to hypergraphs. The result can be seen as a
-- 2-order sequential CRF, with the sequence of nodes corresponding to a
-- top-down, left-to-right traversal. Namely, a hyperarc connects each node pair
-- with its parent and left-sinbling, if present (by node we really mean a pair
-- node/label). Determining a path in such a hypergraph yields a disambiguated
-- tree (in terms of labels).


module NLP.Departage.DepTree.AsHype
  (
  ) where


import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State.Strict as State
import qualified Control.Arrow as Arr

import qualified Pipes as Pipes

import           Data.Maybe (catMaybes)
import qualified Data.Tree as R
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Data.Traversable (traverse)

-- import qualified NLP.Departage.DepTree as Dep
import qualified NLP.Departage.Hype as Hype


-- | A dependency tree to encode (rose representation), with the set of
-- potential labels assigned to each node (note that the goal is to disambiguate
-- over node labels).
type DepTree a b = R.Tree (S.Set a, Maybe b)
-- type DepTree a b = Dep.Tree (S.Set a) b


-- | Encoding result.
data EncHype a b = EncHype
  { encHype :: Hype.Hype
    -- ^ The actually encoded hypergraph
  , nodeLabel :: Hype.Node -> (a, Maybe b)
    -- ^ A label assigned to a hypernode corresponds to (i) the original node
    -- label, and (i) the original label assigned to the arc between the node
    -- and its parent.
  }


-- -- | Encode the given dependency tree as a hypergraph.
-- encodeAsHype
--   :: DepTree a b
--   -> EncHype a b
-- encodeAsHype depTree = undefined


-- encode (Just (parLabMap, mayParArc)) (Just (sisLabSet, maySisArc)) =
encode
  :: Maybe (M.Map Hype.Node a, Maybe b)
  -> Maybe (M.Map Hype.Node a, Maybe b)
  -> (M.Map Hype.Node a, Maybe b)
  -> State.State
     ( M.Map Hype.Node [S.Set Hype.Node]
     , M.Map Hype.Node (a, Maybe b)
     ) ()
encode parent sister this =

  -- flip State.execState (M.empty, M.empty) . Pipes.runListT $ do
  Pipes.runListT $ do
    target <- split this
    lift $ addLabel (fst target) (snd target)
    tail1 <- traverse split parent
    tail2 <- traverse split sister
    lift . addEdge (fst target) . map fst $ catMaybes [tail1, tail2]

  where

    addLabel node label =
      State.modify' . Arr.second $
        M.insert node label

    addEdge target tails =
      State.modify' . Arr.first $
        let alter = Just . \case
              Nothing -> [S.fromList tails]
              Just xs -> S.fromList tails : xs
        in  M.alter alter target

    split (labMap, mayArc) = do
      (nodeID, label) <- each $ M.toList labMap
      return $ (nodeID, (label, mayArc))

    each = Pipes.Select . Pipes.each


-- | Identify the tree in a way that each node/label pair obtains a unique ID
-- (node ID `Hype.Node`).
identifyLabels
  :: R.Tree (S.Set a, Maybe b)
  -> R.Tree (M.Map Hype.Node a, Maybe b)
identifyLabels = undefined


-- | Create a hypergraph from a list of arcs. An abstraction over
-- `Hype.fromList`, which needlessly requires arc IDs. Note that the sets of
-- nodes representing arcs should be distinct.
hypeFromList :: [(Hype.Node, [S.Set Hype.Node])] -> Hype.Hype
hypeFromList xs = undefined
