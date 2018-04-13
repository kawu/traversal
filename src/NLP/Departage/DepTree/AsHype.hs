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
import qualified NLP.Departage.Prob as P


-- | A dependency tree to encode (rose representation), with the set of
-- potential labels assigned to each node (note that the goal is to disambiguate
-- over node labels).
-- type DepTree a b = R.Tree (S.Set a, Maybe b)
type DepTree a b = R.Tree (P.Prob a, Maybe b)


-- | Encoding result.
data EncHype a b = EncHype
  { encHype :: Hype.Hype
    -- ^ The actually encoded hypergraph
  , arcProb :: Hype.Arc -> Double
    -- ^ Arc probabilities
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
    putStr (show $ Hype.tail arc encHype)
    putStr " ("
    putStr (show $ arcProb arc)
    putStrLn ")"


-- | Encode the given dependency tree as a hypergraph.
-- TODO: should there be other features related to dependency tree leaves?
encodeAsHype
  :: DepTree a b
  -> EncHype a b
encodeAsHype depTree =

  EncHype
  { encHype = encodedHype
  , arcProb = \x -> arcProbMap M.! x
  , nodeLabel = \x -> labMap M.! x
  }

  where

    (encodedHype, arcProbMap) = hypeFromList . M.toList $ fmap P.fromList arcMap

    (arcMap, labMap) = flip State.execState (M.empty, M.empty) $ do
      go Nothing Nothing [identifyLabels depTree]

    go parent sister (tree : forest) = do
      let treeLabel = R.rootLabel tree
      encode parent sister treeLabel
      go (Just treeLabel) Nothing (R.subForest tree)
      go parent (Just treeLabel) forest
    go _ _ [] = return ()


-- | A mapping from hypergraph nodes to labels and the corresponding
-- probabilities.
type NodeMap a = M.Map Hype.Node (a, Double)


-- encode (Just (parLabMap, mayParArc)) (Just (sisLabSet, maySisArc)) =
encode
  :: Maybe (NodeMap a, Maybe b)
  -> Maybe (NodeMap a, Maybe b)
  -> (NodeMap a, Maybe b)
  -> State.State
     ( M.Map Hype.Node [(S.Set Hype.Node, Double)]
       -- ^ For each hypernode, the list of incoming arcs and their probabilities
     , M.Map Hype.Node (a, Maybe b)
       -- ^ For each hypernode, the corresponding label pair (node label, parent label)
     ) ()
encode parent sister this =

  -- flip State.execState (M.empty, M.empty) . Pipes.runListT $ do
  Pipes.runListT $ do
    target <- split this
    lift $ addLabel (fst target) (snd target)
    tail1 <- Trav.traverse split parent
    tail2 <- Trav.traverse split sister
    lift . addEdge target $ catMaybes [tail1, tail2]

  where

    addLabel node (nodeLabel, _prob, mayArcLabel) =
      State.modify' . Arr.second $
        M.insert node (nodeLabel, mayArcLabel)

    addEdge (targetNode, (_, targetProb, _)) tails = do
      let tailNodes =          map (\(node, (_, _prob, _)) -> node) tails
          tailProb = product $ map (\(_node, (_, prob, _)) -> prob) tails
          tailAll = (S.fromList tailNodes, tailProb * targetProb)
      State.modify' . Arr.first $
        let alter = Just . \case
              Nothing -> [tailAll]
              Just xs -> tailAll : xs
        in  M.alter alter targetNode

    split (labMap, mayArc) = do
      (nodeID, (label, prob)) <- each $ M.toList labMap
      return $ (nodeID, (label, prob, mayArc))

    each = Pipes.Select . Pipes.each


-- | Identify the tree in a way that each node/label pair obtains a unique ID
-- (node ID `Hype.Node`).
identifyLabels
  -- :: R.Tree (S.Set a, b)
  :: R.Tree (P.Prob a, b)
  -> R.Tree (NodeMap a, b)
identifyLabels =
  flip State.evalState 0 . Trav.traverse identify
  where
    identify (labProb, other) = do
      i0 <- State.get
      let labMap = M.fromList
            [ (Hype.Node i, lp)
            | (i, lp) <- zip [i0..] (P.toList labProb)
            ]
      State.put (i0 + M.size (P.unProb labProb))
      return (labMap, other)


-- | Create an arc-weighted hypergraph from a list of arcs. An abstraction over
-- `Hype.fromList`, which needlessly requires arc IDs.
hypeFromList
  :: [(Hype.Node, P.Prob (S.Set Hype.Node))]
  -> (Hype.Hype, M.Map Hype.Arc Double)
-- hypeFromList :: [(Hype.Node, [S.Set Hype.Node])] -> Hype.Hype
hypeFromList input =
  ( Hype.fromList . map (Arr.second $ fmap fst) $ hypeList
  , M.unions . map (fmap snd . snd) $ hypeList
  )
  where
    hypeList = snd $ L.mapAccumL update 0 input
    update p (target, tails) =
      let tails' = M.fromList
            [ (Hype.Arc i, tailAndProb)
            | (i, tailAndProb) <- zip [p..] (P.toList tails) ]
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
    binary = P.fromList [(False, 0.5), (True, 0.5)]
    mkT xs = Dep.Tree
      { Dep.root = binary
      , children = map (,()) xs }
    mkL = mkT []
