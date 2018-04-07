{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}


-- | Transforming dependency trees to hypergraphs. The result can be seen as a
-- 2-order sequential CRF, with the sequence of nodes corresponding to a
-- top-down, left-to-right traversal. Namely, a hyperarc connects each node pair
-- with its parent and left-sinbling, if present (by node we really mean a pair
-- node/label). Determining a path in such a hypergraph yields a disambiguated
-- tree (in terms of labels).


module NLP.Departage.DepTree.AsHype
  ( DepTree
  , EncHype
  , encodeAsHype

  -- * Temporary
  , testAsHype
  ) where


import           Control.Monad (forM_)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State.Strict as State
import qualified Control.Arrow as Arr

import qualified Pipes as Pipes

import           Data.Maybe (catMaybes)
import qualified Data.List as L
import qualified Data.Tree as R
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Traversable as Trav

import qualified NLP.Departage.Hype as Hype
import qualified NLP.Departage.DepTree as Dep


-- | A dependency tree to encode (rose representation), with the set of
-- potential labels assigned to each node (note that the goal is to disambiguate
-- over node labels).
type DepTree a b = R.Tree (S.Set a, Maybe b)


-- | Encoding result.
data EncHype a b = EncHype
  { encHype :: Hype.Hype
    -- ^ The actually encoded hypergraph
  , nodeLabel :: Hype.Node -> (a, Maybe b)
    -- ^ A label assigned to a hypernode corresponds to (i) the original node
    -- label, and (i) the original label assigned to the arc between the node
    -- and its parent.
  }


printEncHype
  :: (Show a, Show b)
  => EncHype a b
  -> IO ()
printEncHype EncHype{..} = do
  putStrLn "# Nodes"
  forM_ (S.toList $ Hype.nodes encHype) $ \node -> do
    putStr (show node)
    print $ nodeLabel node
  putStrLn "# Arcs"
  forM_ (S.toList $ Hype.arcs encHype) $ \arc -> do
    putStr (show $ Hype.head arc encHype)
    putStr " <= "
    putStrLn (show $ Hype.tail arc encHype)


-- | Encode the given dependency tree as a hypergraph.
-- TODO: should there be other features related to dependency tree leaves?
encodeAsHype
  :: DepTree a b
  -> EncHype a b
encodeAsHype depTree =

  EncHype
  { encHype = hypeFromList (M.toList arcMap)
  , nodeLabel = \x -> labMap M.! x
  }

  where

    (arcMap, labMap) = flip State.execState (M.empty, M.empty) $ do
      go Nothing Nothing [identifyLabels depTree]

    go parent sister (tree : forest) = do
      let treeLabel = R.rootLabel tree
      encode parent sister treeLabel
      go (Just treeLabel) Nothing (R.subForest tree)
      go parent (Just treeLabel) forest
    go _ _ [] = return ()


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
    tail1 <- Trav.traverse split parent
    tail2 <- Trav.traverse split sister
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
  :: R.Tree (S.Set a, b)
  -> R.Tree (M.Map Hype.Node a, b)
identifyLabels =
  flip State.evalState 0 . Trav.traverse identify
  where
    identify (labSet, other) = do
      i0 <- State.get
      let labMap = M.fromList
            [ (Hype.Node i, lab)
            | (i, lab) <- zip [i0..] (S.toList labSet)
            ]
      State.put (i0 + S.size labSet)
      return (labMap, other)


-- | Create a hypergraph from a list of arcs. An abstraction over
-- `Hype.fromList`, which needlessly requires arc IDs. Note that the sets of
-- nodes representing arcs should be distinct.
hypeFromList :: [(Hype.Node, [S.Set Hype.Node])] -> Hype.Hype
hypeFromList =
  Hype.fromList . snd . L.mapAccumL update 0
  where
    update p (target, tails) =
      let tails' = M.fromList
            [ (Hype.Arc i, tailSet)
            | (i, tailSet) <- zip [p..] tails ]
          q = p + length tails'
      in  (q, (target, tails'))


-----------------------------------
-- TEST
-----------------------------------


testAsHype :: IO ()
testAsHype = do
  let
    -- depTree = mkT [mkL, mkT [mkL, mkT [mkL], mkL]]
    depTree = mkT [mkL, mkL, mkL]
    encHype = encodeAsHype (Dep.toRose depTree)
  printEncHype encHype
  where
    binary = S.fromList [False, True]
    mkT xs = Dep.Tree
      { Dep.root = binary
      , children = map (,()) xs }
    mkL = mkT []
