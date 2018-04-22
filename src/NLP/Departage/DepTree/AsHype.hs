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
  , EncHype (..)
  , Label (..)
  , encodeTree
  , decodeTree
  , decodeTree'

  -- * Temporary
  , testAsHype
  , printEncHype
  ) where


import           Control.Monad (forM, forM_, guard)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State.Strict as State
-- import qualified Control.Arrow as Arr

import qualified Pipes as Pipes

-- import           Data.Maybe (catMaybes)
import qualified Data.List as L
import qualified Data.Tree as R
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Traversable as Trav

import qualified NLP.Departage.CRF.Map as CRF
import qualified NLP.Departage.CRF as CRF

import qualified NLP.Departage.Hype as Hype
import qualified NLP.Departage.DepTree as Dep
import qualified NLP.Departage.Prob as P

-- import Debug.Trace (trace)


-----------------------------------
-- Types
-----------------------------------


-- | A dependency tree to encode (rose representation), with the set of
-- potential labels assigned to each node (note that the goal is to disambiguate
-- over node labels).
type DepTree a b = R.Tree (P.Prob a, b)


-- -- | Encoding result.
-- data EncHype a b = EncHype
--   { encHype :: Hype.Hype
--     -- ^ The actually encoded hypergraph
--   , arcProb :: Hype.Arc -> Double
--     -- ^ Arc probabilities
--   , nodeLabel :: Hype.Node -> (Label a, b)
--     -- ^ A label assigned to a hypernode corresponds to (i) enriched original
--     -- node label, and (ii) the original label assigned to the token/arc between
--     -- the node and its parent.
--   , nodeParent :: Hype.Node -> Maybe b
--     -- ^ Original token/arc label assigned to node's parent
--   }


-- | Encoding result.  New, alternative form.
data EncHype a b = EncHype
  { encTree :: R.Tree DepNodeID
    -- ^ The tree of IDs

  , encToken :: DepNodeID -> b
    -- ^ Lexical item corresponding to the given tree node

  , encHype :: Hype.Hype
    -- ^ The actually encoded hypergraph

  , encInterps :: DepNodeID -> S.Set Hype.Node
    -- ^ To each tree node, a set of hypernodes is assigned, which represent
    -- the possible interpretations (or values) of the corresponding node

  , encNode :: Hype.Node -> DepNodeID
    -- ^ Inverse of `encInterps`

  , encLabel :: Hype.Node -> Label a
    -- ^ An enriched label assigned to a hypernode

  , encArcProb :: Hype.Arc -> Double
    -- ^ Arc probabilities

  -- TODO: this is too specific and should be done a different way
  , encParent :: Hype.Node -> Maybe b
    -- ^ Original token/arc label assigned to node's parent
  }


-- | ID of a node in a dependency tree.
type DepNodeID = Int


-- | Complex label representation.
data Label a = Label
  { origLabel :: a
    -- ^ The original label assigned to the given hypergraph node
  , parentLabel :: Maybe a
    -- ^ Label assigned to the parent (if any); performs the function of a
    -- memory cell
--   , arcLabel :: b
--     -- ^ The original label assigned to the arc between the node and its parent
  } deriving (Show, Eq, Ord)


-- | A mapping from hypergraph nodes to labels and the corresponding
-- probabilities.
type NodeMap a = M.Map Hype.Node (Label a, Double)


-- | Node type marker.
data NodeTyp
  = Parent
  | Sister
  deriving (Show, Eq, Ord)


-----------------------------------
-- Encoding
-----------------------------------


data EncodingState a b = EncodingState
  { encIncomingMap :: M.Map Hype.Node [S.Set Hype.Node]
    -- ^ For each hypernode, the list of incoming arcs
  -- , encLabelMap :: M.Map Hype.Node (Label a, Double, b)
  , encLabelMap :: M.Map Hype.Node (Label a, Double)
    -- ^ For each hypernode, the corresponding label and probability
  , encTokenMap :: M.Map DepNodeID b
    -- ^ For each hypernode, the corresponding label pair (node label, arc
    -- label) and the corresponding probability (in the middle)
  , encParentMap :: M.Map Hype.Node (Maybe b)
    -- ^ Map which assigns to each node the token/arc label assigned to its
    -- parent
  , encInterpsMap :: M.Map DepNodeID (S.Set Hype.Node)
    -- ^ Map which assigns to each node the token/arc label assigned to its
    -- parent
  , encCounter :: Int
    -- ^ Counter for generating new node IDs
  }


newEncodingState :: EncodingState a b
newEncodingState = EncodingState
  { encIncomingMap = M.empty
  , encLabelMap = M.empty
  , encTokenMap = M.empty
  , encParentMap = M.empty
  , encInterpsMap = M.empty
  , encCounter = 0
  }


onIncomingMap
  :: (M.Map Hype.Node [S.Set Hype.Node] -> M.Map Hype.Node [S.Set Hype.Node])
  -- :: ([(S.Set Hype.Node, S.Set Hype.Node)] -> [(S.Set Hype.Node, S.Set Hype.Node)])
  -> EncodingState a b -> EncodingState a b
onIncomingMap f st = st {encIncomingMap = f (encIncomingMap st)}


onLabelMap
  :: (M.Map Hype.Node (Label a, Double) -> M.Map Hype.Node (Label a, Double))
  -> EncodingState a b -> EncodingState a b
onLabelMap f st = st {encLabelMap = f (encLabelMap st)}


onTokenMap
  :: (M.Map DepNodeID b -> M.Map DepNodeID b)
  -> EncodingState a b -> EncodingState a b
onTokenMap f st = st {encTokenMap = f (encTokenMap st)}


onInterpsMap
  :: (M.Map DepNodeID (S.Set Hype.Node) -> M.Map DepNodeID (S.Set Hype.Node))
  -> EncodingState a b -> EncodingState a b
onInterpsMap f st = st {encInterpsMap = f (encInterpsMap st)}


onCounter :: (Int -> Int) -> EncodingState a b -> EncodingState a b
onCounter f st = st {encCounter = f (encCounter st)}


onParentMap
  :: (M.Map Hype.Node (Maybe b) -> M.Map Hype.Node (Maybe b))
  -> EncodingState a b -> EncodingState a b
onParentMap f st = st {encParentMap = f (encParentMap st)}


-- | Encode the given dependency tree as a hypergraph.
-- TODO: should there be other features related to dependency tree leaves?
encodeTree
  :: (Ord a)
  => DepTree a b
  -> EncHype a b
encodeTree depTree =

  EncHype
  { encTree = fmap snd depTreeWithIDs
  , encToken = \x -> encTokenMap M.! x
  , encHype = encodedHype
  , encInterps = \x -> encInterpsMap M.! x
  , encNode = \x -> interpsMapRev M.! x
  , encLabel = \x ->
      let (a, _) = encLabelMap M.! x
      in  a
  , encArcProb = \arc ->
      let
        hd = Hype.head arc encodedHype
        tl = S.toList (Hype.tail arc encodedHype)
      in
        nodeProb hd * product (map nodeProb tl)
  , encParent = \x -> encParentMap M.! x
  }

  where

    depTreeWithIDs = identifyTree depTree
    encodedHype = hypeFromList (M.toList encIncomingMap)
    nodeProb node = snd (encLabelMap M.! node)
    interpsMapRev = M.fromList
      [ (nodeID, depNodeID)
      | (depNodeID, nodeSet) <- M.toList encInterpsMap
      , nodeID <- S.toList nodeSet ]

    -- EncodingState arcMap labMap tokMap parMap _ =
    EncodingState{..} =
      flip State.execState newEncodingState $ do
        goR depTreeWithIDs

    goR tree = goF Nothing Nothing [tree]

    goF parProb parLabel (tree:forest) = do
      let ((tokProb, tokLabel), rootID) = R.rootLabel tree
      addToken rootID tokLabel
      nodeMap <- mkNodeMap parProb tokProb
      childMapMay <- goF (Just tokProb) (Just tokLabel) (R.subForest tree)
      sisterMapMay <- goF parProb parLabel forest
      encodeEdge nodeMap childMapMay sisterMapMay
      addInterps rootID (M.keysSet nodeMap)
      forM_ (M.toList nodeMap) $ \(node, (label, prob)) -> do
        addLabel node (label, prob)
        addParentLabel node parLabel
      return $ Just nodeMap
    goF _ _ [] = return Nothing

--     goF prevLabel prevTyp (tree : forest) = do
--       let treeLabel = R.rootLabel tree
--       treeLabel' <- encode2 prevLabel prevTyp treeLabel
--       goF treeLabel' Parent (R.subForest tree)
--       goF treeLabel' Sister forest
--     goF _ _ [] = return ()


-- | Assign a unique ID (1, 2, ..) to each node in the tree.
identifyTree :: R.Tree a -> R.Tree (a, Int)
identifyTree =
  snd . Trav.mapAccumL update 1
  where
    update k x = (k+1, (x, k))


-- | Create a node map.
mkNodeMap
  :: Maybe (P.Prob a)
     -- ^ Probability map of the parent node
  -> P.Prob a
     -- ^ Probability map of the current node
  -> State.State (EncodingState a b) (NodeMap a)
mkNodeMap Nothing this = do
  xs <- forM (P.toList this) $ \(orig, prob) -> do
    node <- newNode
    let nodeLabel = Label {origLabel=orig, parentLabel=Nothing}
    return (node, (nodeLabel, prob))
  return (M.fromList xs)
mkNodeMap (Just parent) this = do
  xss <- forM (P.toList this) $ \(thisOrig, thisProb) -> do
    forM (P.toList parent) $ \(parOrig, parProb) -> do
      node <- newNode
      let nodeLabel = Label {origLabel=thisOrig, parentLabel=Just parOrig}
      return (node, (nodeLabel, thisProb * parProb))
  return (M.fromList $ concat xss)


-- | Encode the edge.
encodeEdge
  :: (Eq a)
  => NodeMap a
     -- ^ Node map of the current node
  -> Maybe (NodeMap a)
     -- ^ Node map of the child
  -> Maybe (NodeMap a)
     -- ^ Node map of the sister
  -> State.State (EncodingState a b) ()
encodeEdge nodeMap (Just childMap) (Just sisterMap) = Pipes.runListT $ do
  (node, (nodeLabel, _)) <- each (M.toList nodeMap)
  (child, (childLabel, _)) <- each (M.toList childMap)
  guard $ parentLabel childLabel == Just (origLabel nodeLabel)
  (sister, (sisterLabel, _)) <- each (M.toList sisterMap)
  guard $ parentLabel sisterLabel == parentLabel nodeLabel
  lift $ addEdge [child, sister] node
encodeEdge nodeMap (Just childMap) Nothing = Pipes.runListT $ do
  (node, (nodeLabel, _)) <- each (M.toList nodeMap)
  (child, (childLabel, _)) <- each (M.toList childMap)
  guard $ parentLabel childLabel == Just (origLabel nodeLabel)
  lift $ addEdge [child] node
encodeEdge nodeMap Nothing (Just sisterMap) = Pipes.runListT $ do
  (node, (nodeLabel, _)) <- each (M.toList nodeMap)
  (sister, (sisterLabel, _)) <- each (M.toList sisterMap)
  guard $ parentLabel sisterLabel == parentLabel nodeLabel
  lift $ addEdge [sister] node
encodeEdge nodeMap Nothing Nothing = Pipes.runListT $ do
  (node, (_nodeLabel, _)) <- each (M.toList nodeMap)
  lift $ addEdge [] node


-- encode1
--   :: (P.Prob a, b)
--      -- ^ Probability map of the current node
--   -> State.State (EncodingState a b) (NodeMap a, b)
-- encode1 this = do
--   xs <- forM (P.toList $ fst this) $ \(orig, prob) -> do
--     node <- newNode
--     let nodeLabel = Label {origLabel=orig, parentLabel=Nothing}
--     addLabel node (nodeLabel, prob, snd this)
--     addEdge [node] []
--     return (node, (nodeLabel, prob))
--   return (M.fromList xs, snd this)


-- encode2
--   :: (Ord a)
--   => (NodeMap a, b)
--      -- ^ Node map corresponding to the previous node
--   -> NodeTyp
--      -- ^ Type of the previous node
--   -> (P.Prob a, b)
--      -- ^ Probability map of the current node
--   -> State.State (EncodingState a b) (NodeMap a, b)
-- encode2 prev prevTyp this = do
--
--   xss <- forM (P.toList $ fst this) $ \(orig, prob) -> do
--     forM (split prev) $ \source -> do
--       let
--         sourceLabel = _1 (snd source)
--         parentLab =
--           case prevTyp of
--             Sister -> parentLabel sourceLabel
--             Parent -> Just (origLabel sourceLabel)
--         nodeLabel = Label {origLabel=orig, parentLabel=parentLab}
--         nodeProb = prob * _2 (snd source)
--       return (nodeLabel, (nodeProb, S.singleton $ fst source))
--
--   let
--     merge (prob1, tail1) (prob2, tail2) = (prob1 + prob2, S.union tail1 tail2)
--     elemMap = M.fromListWith merge (concat xss)
--
--   xs <- forM (M.toList elemMap) $ \(nodeLabel, (nodeProb, sourceSet)) -> do
--     node <- newNode
--     addLabel node (nodeLabel, nodeProb, snd this)
--     forM_ (S.toList sourceSet) $ \source ->
--       addEdge [node] [source]
--     return (node, (nodeLabel, nodeProb))
--
--   return (M.fromList xs, snd this)


newNode :: State.State (EncodingState a b) Hype.Node
newNode = do
  x <- State.gets encCounter
  State.modify' $ onCounter (+1)
  return (Hype.Node x)


addToken :: DepNodeID -> b -> State.State (EncodingState a b) ()
addToken node x =
  State.modify' . onTokenMap $
    M.insert node x


addInterps :: DepNodeID -> S.Set Hype.Node -> State.State (EncodingState a b) ()
addInterps node interpSet =
  State.modify' . onInterpsMap $
    M.insertWith S.union node interpSet


addLabel
  :: Hype.Node
  -> (Label a, Double)
  -> State.State (EncodingState a b) ()
addLabel node x =
  State.modify' . onLabelMap $
    M.insert node x


addParentLabel
  :: Hype.Node
  -> Maybe b
  -> State.State (EncodingState a b) ()
addParentLabel node x =
  State.modify' . onParentMap $
    M.insert node x


addEdge
  :: [Hype.Node]
  -> Hype.Node
  -> State.State (EncodingState a b) ()
addEdge tailNodes targetNode = do
  let tailAll = S.fromList tailNodes
  State.modify' . onIncomingMap $
    let alter = Just . \case
          Nothing -> [tailAll]
          Just xs -> tailAll : xs
    in  M.alter alter targetNode


-- addEdge
--   :: [Hype.Node]
--   -> [Hype.Node]
--   -> State.State (EncodingState a b) ()
-- addEdge tailNodes targetNodes = do
--   let
--     targetAll = S.fromList targetNodes
--     tailAll = S.fromList tailNodes
--     newElem = (targetAll, tailAll)
--   State.modify' . onIncomingMap $ (newElem:)


-- split
--   :: (NodeMap a, b)
--   -> Pipes.ListT
--      (State.State (EncodingState a b))
--      (Hype.Node, (Label a, Double, b))
-- split (labMap, mayArc) = do
--   (nodeID, (label, prob)) <- each $ M.toList labMap
--   return $ (nodeID, (label, prob, mayArc))


-- split
--   :: (NodeMap a, b)
--   -> [(Hype.Node, (Label a, Double, b))]
-- split (labMap, mayArc) = do
--   (nodeID, (label, prob)) <- M.toList labMap
--   return $ (nodeID, (label, prob, mayArc))


-- -- encode (Just (parLabMap, mayParArc)) (Just (sisLabSet, maySisArc)) =
-- encode
--   :: Maybe (NodeMap a, b)
--   -> Maybe (NodeMap a, b)
--   -> (NodeMap a, b)
--   -> State.State
--      ( M.Map Hype.Node [(S.Set Hype.Node, Double)]
--        -- ^ For each hypernode, the list of incoming arcs and their probabilities
--      , M.Map Hype.Node (Label a, b)
--        -- ^ For each hypernode, the corresponding label pair (node label, arc label)
--      ) ()
-- encode parent sister this =
--
--   -- flip State.execState (M.empty, M.empty) . Pipes.runListT $ do
--   Pipes.runListT $ do
--     target <- split this
--     lift $ addLabel (fst target) (snd target)
--     tail1 <- Trav.traverse split parent
--     tail2 <- Trav.traverse split sister
--     lift . addEdge target $ catMaybes [tail1, tail2]
--
--   where
--
--     addLabel node (nodeLabel, _prob, mayArcLabel) =
--       State.modify' . Arr.second $
--         M.insert node (nodeLabel, mayArcLabel)
--
--     addEdge (targetNode, (_, targetProb, _)) tails = do
--       let tailNodes =          map (\(node, (_, _prob, _)) -> node) tails
--           tailProb = product $ map (\(_node, (_, prob, _)) -> prob) tails
--           tailAll = (S.fromList tailNodes, tailProb * targetProb)
--       State.modify' . Arr.first $
--         let alter = Just . \case
--               Nothing -> [tailAll]
--               Just xs -> tailAll : xs
--         in  M.alter alter targetNode
--
--     split (labMap, mayArc) = do
--       (nodeID, (label, prob)) <- each $ M.toList labMap
--       return $ (nodeID, (label, prob, mayArc))
--
--     each = Pipes.Select . Pipes.each


-- -- | Identify the tree in a way that each node/label pair obtains a unique ID
-- -- (node ID `Hype.Node`).
-- identifyLabels
--   -- :: R.Tree (S.Set a, b)
--   :: R.Tree (P.Prob a, b)
--   -> R.Tree (NodeMap a, b)
-- identifyLabels = undefined
-- --   flip State.evalState 0 . Trav.traverse identify
-- --   where
-- --     identify (labProb, other) = do
-- --       i0 <- State.get
-- --       let labMap = M.fromList
-- --             [ (Hype.Node i, lp)
-- --             | (i, lp) <- zip [i0..] (P.toList labProb)
-- --             ]
-- --       State.put (i0 + M.size (P.unProb labProb))
-- --       return (labMap, other)


-- -- | Create an arc-weighted hypergraph from a list of arcs. An abstraction over
-- -- `Hype.fromList`, which needlessly requires arc IDs.
-- hypeFromList
--   :: [(Hype.Node, P.Prob (S.Set Hype.Node))]
--   -> (Hype.Hype, M.Map Hype.Arc Double)
-- hypeFromList input =
--   ( Hype.fromList . map (Arr.second $ fmap fst) $ hypeList
--   , M.unions . map (fmap snd . snd) $ hypeList
--   )
--   where
--     hypeList = snd $ L.mapAccumL update 0 input
--     update p (target, tails) =
--       let tails' = M.fromList
--             [ (Hype.Arc i, tailAndProb)
--             | (i, tailAndProb) <- zip [p..] (P.toList tails) ]
--           q = p + length tails'
--       in  (q, (target, tails'))


-- -- | Create a hypergraph from a list of arcs. An abstraction over
-- -- `Hype.fromList`, which needlessly requires arc IDs.
-- hypeFromList :: [(S.Set Hype.Node, S.Set Hype.Node)] -> Hype.Hype
-- hypeFromList =
--   Hype.fromList . snd . L.mapAccumL update 0
--   where
--     update i (target, tails) =
--       ( i + 1
--       , (target, Hype.Arc i, tails)
--       )


-- | Create an arc-weighted hypergraph from a list of arcs. An abstraction over
-- `Hype.fromList`, which needlessly requires arc IDs.
hypeFromList
  :: [(Hype.Node, [S.Set Hype.Node])]
  -> Hype.Hype
hypeFromList input =
  Hype.fromList hypeList
  where
    hypeList = snd $ L.mapAccumL update 0 input
    update p (target, tails) =
      let tails' = M.fromList
            [ (Hype.Arc i, tl)
            | (i, tl) <- zip [p..] tails ]
          q = p + length tails'
      in  (q, (target, tails'))


-----------------------------------
-- Decoding
-----------------------------------


-- | Perform decoding, i.e., assign labels to the individual dependency nodes
-- based on the given node probabilities.
decodeTree
  :: (Ord a, CRF.Flo v)
  => EncHype a b
     -- ^ Its encoded version
  -> CRF.Prob Hype.Node v
     -- ^ Node probabilities (e.g. marginals)
  -> DepTree a b
decodeTree EncHype{..} nodeProb =
  fmap decodeNode encTree
  where
    decodeNode depNodeID =
      let
        prob = P.fromList
          [ (origLabel, CRF.toDouble $ nodeProb hypeNodeID)
          | hypeNodeID <- S.toList (encInterps depNodeID)
          , let Label{..} = encLabel hypeNodeID
          ]
        tok = encToken depNodeID
      in
        (prob, tok)


-- | A version of `decodeTree` based on pre-selected nodes.
decodeTree'
  :: (Ord a)
  => EncHype a b
     -- ^ Its encoded version
  -> S.Set Hype.Node
     -- ^ Pre-selected nodes
  -> DepTree a b
decodeTree' EncHype{..} nodeSet =
  fmap decodeNode encTree
  where
    decodeNode depNodeID =
      let
        prob = P.fromList $ do
          hypeNodeID <- S.toList (encInterps depNodeID)
          let Label{..} = encLabel hypeNodeID
              nodeProb = if S.member hypeNodeID nodeSet then 1 else 0
          return (origLabel, nodeProb)
        tok = encToken depNodeID
      in
        (prob, tok)


-----------------------------------
-- Utils
-----------------------------------


_1 :: (a, b, c) -> a
_1 (x, _, _) = x


_2 :: (a, b, c) -> b
_2 (_, x, _) = x


_3 :: (a, b, c) -> c
_3 (_, _, x) = x


each :: (Monad m) => [a] -> Pipes.ListT m a
each = Pipes.Select . Pipes.each


printEncHype
  :: (Show a, Show b)
  => EncHype a b
  -> IO ()
printEncHype EncHype{..} = do
  putStrLn "# Nodes"
  forM_ (S.toList $ Hype.nodes encHype) $ \node -> do
    putStr (show node)
    print $ encLabel node
  putStrLn "# Arcs"
  forM_ (S.toList $ Hype.arcs encHype) $ \arc -> do
    putStr (show $ Hype.head arc encHype)
    putStr " <= "
    putStr (show $ Hype.tail arc encHype)
    putStr " ("
    putStr (show $ encArcProb arc)
    putStrLn ")"


-----------------------------------
-- TEST
-----------------------------------


testAsHype :: IO ()
testAsHype = do
  let
    -- depTree = mkT [mkL, mkT [mkL, mkT [mkL], mkL]]
    depTree = mkT [mkL, mkL, mkL]
    encHype = encodeTree (Dep.toRose depTree)
  printEncHype encHype
  where
    binary = P.fromList [(False, 0.5), (True, 0.5)]
    mkT xs = Dep.Tree
      { Dep.root = binary
      , children = map (,()) xs }
    mkL = mkT []
