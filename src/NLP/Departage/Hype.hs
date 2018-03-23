{-# LANGUAGE RecordWildCards #-}

-- | An implementation of a hypergraph.


module NLP.Departage.Hype
(
-- * Types
  Hype
, Node (..)
, Arc (..)

-- * Primitive operations
, nodes
, arcs
, head
, tail
, incoming
, outgoing

-- * Intermediate operations
, final

-- * Construction
, fromList

-- * Tests
-- , testHype


-- -- * Types
--   DAG
-- , NodeID (..)
-- , EdgeID (..)
-- , Edge (..)

-- -- * Primitive Operations
-- , begsWith
-- , endsWith
-- , incomingEdges
-- , outgoingEdges
-- , maybeNodeLabel
-- , nodeLabel
-- , maybeEdgeLabel
-- , edgeLabel
--
-- -- * Intermediate Operations
-- , prevEdges
-- , isInitialEdge
-- -- , isInitialNode
-- , nextEdges
-- , isFinalEdge
--
-- , minEdge
-- , maxEdge
--
-- , mapN
-- , mapE
-- , zipE
--
-- -- * Advanced Operations
-- , dagNodes
-- , dagEdges
--
-- -- * Conversion
-- , fromList
-- , fromList'
-- , fromEdgesUnsafe
-- -- -- ** Provisional
-- -- , toListProv
--
-- -- * Check
-- , isOK
) where


import Prelude hiding (head, tail)
-- import           Control.Applicative ((<$>))
-- import qualified Data.Foldable as F
-- import qualified Data.Traversable as T
-- import qualified Data.Array as A
-- -- import qualified Data.Vector as V

-- import           Data.Maybe (maybeToList)
import           Data.Ix (Ix)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

-- import Data.Binary (Binary, get, put, putWord8, getWord8)
-- import Data.Vector.Binary ()
-- import qualified Data.Binary as B


------------------------------------------------------------------
-- Types
------------------------------------------------------------------


-- | A hypergraph parametrized by the type of nodes `i`, the type of arcs `j`.
data Hype = Hype
  { nodeMap :: M.Map Node NodeStr
    -- ^ The set of underlying nodes
  , arcMap  :: M.Map Arc ArcStr
  } deriving (Show, Eq, Ord)


-- | A node.  We use a predetermined type because
-- data-memocombinators do not work with any `Ord` instances.
newtype Node = Node {unNode :: Int}
  deriving (Show, Read, Eq, Ord, Ix)


-- | An arc.  We use a predetermined type because
-- data-memocombinators do not work with any `Ord` instances.
newtype Arc = Arc {unArc :: Int}
  deriving (Show, Read, Eq, Ord, Ix)


-- | Node internal structure.
data NodeStr = NodeStr
  { insSet :: S.Set Arc
    -- ^ Set of incoming arcs
  , outSet :: S.Set Arc
    -- ^ Set of outgoing arcs
  } deriving (Show, Eq, Ord)


-- | Arc internal structure.
data ArcStr = ArcStr
  { arcHead :: Node
  , arcTail :: S.Set Node
  } deriving (Show, Eq, Ord)


-- | Retrieve the set of all nodes.
nodes :: Hype -> S.Set Node
nodes = M.keysSet . nodeMap


-- | Retrieve the set of all arcs.
arcs :: Hype -> S.Set Arc
arcs hype = S.unions
  [ incoming i hype
  | i <- S.toList $ nodes hype ]


-- | A head of a given edge.
head :: Arc -> Hype -> Node
head j =
  maybe (error errMsg) arcHead . M.lookup j . arcMap
  where errMsg = "Hype.head: unknown arc"


-- | A tail of a given edge, which is basically a set of nodes.
tail :: Arc -> Hype -> S.Set Node
tail j =
  maybe (error errMsg) arcTail . M.lookup j . arcMap
  where errMsg = "Hype.tail: unknown arc"


-- | The set of arcs incoming to a given node.
incoming :: Node -> Hype -> S.Set Arc
incoming i =
  maybe (error errMsg) insSet . M.lookup i . nodeMap
  where errMsg = "Hype.incoming: unknown node"


-- | The set of arcs outgoing from a given node.
outgoing :: Node -> Hype -> S.Set Arc
outgoing i =
  maybe (error errMsg) outSet . M.lookup i . nodeMap
  where errMsg = "Hype.outgoing: unknown node"


------------------------------------------------------------------
-- Intermediate
------------------------------------------------------------------


-- | Return the set of final node.
--
-- TODO: reimplement by using `outgoing`.
--
-- WARNING: in practice, we might need to make sure that these nodes
-- actually represent valid solutions, and not just parsing
-- dead-ends.
final :: Hype -> S.Set Node
final hype =
  nodes hype `S.difference` nonFinal
  where
    nonFinal = S.unions $ do
      i <- S.toList (nodes hype)
      j <- S.toList (incoming i hype)
      return $ tail j hype


------------------------------------------------------------------
-- Construction
------------------------------------------------------------------


-- | Create a hypergraph from a list of nodes together with the incoming arcs.
fromList :: [(Node, M.Map Arc (S.Set Node))] -> Hype
fromList xs = Hype
  newNodeMap
  newArcMap
  where
    newArcMap = M.fromList
      [ (arc, ArcStr {arcHead = node, arcTail = tail'})
      | (node, ingo) <- xs
      , (arc, tail') <- M.toList ingo ]
    newNodeMap = M.fromList
      [ (node, NodeStr {insSet = insgo, outSet = outgo})
      | (node, insgo) <- M.toList insNodeMap
      , let outgo = maybe S.empty id (M.lookup node outNodeMap) ]
    insNodeMap = M.fromList
      [ (node, M.keysSet ingo)
      | (node, ingo) <- xs ]
    outNodeMap = M.fromListWith S.union
      [ (node, S.singleton arc)
      | (arc, ArcStr{..}) <- M.toList newArcMap
      , node <- S.toList arcTail ]


------------------------------------------------------------------
-- Tests
------------------------------------------------------------------


-- testHype :: Hype
-- testHype = fromList
--   [ ( Node 1, M.empty )
--   , ( Node 2, M.empty )
--   , ( Node 3, M.fromList
--       [ (Arc 1, S.fromList [Node 1])
--       , (Arc 2, S.fromList [Node 1, Node 2]) ]
--     )
--   , ( Node 4, M.fromList
--       [ (Arc 3, S.fromList [Node 2, Node 3]) ]
--     )
--   ]


-- -- | A directed acyclic graph (DAG) with nodes of type `a` and
-- -- edges of type `b`.
-- data DAG a b = DAG
--   { nodeMap :: M.Map NodeID (Node a)
--   , edgeMap :: M.Map EdgeID (Edge b)
--   } deriving (Functor, F.Foldable, T.Traversable)
--
-- instance (Binary a, Binary b) => Binary (DAG a b) where
--   put = undefined
--   get = undefined
--
--
-- -- | Node ID.
-- newtype NodeID = NodeID {unNodeID :: Int}
--   deriving (Show, Eq, Ord)
--
--
-- -- | Node of the DAG.
-- data Node a = Node
--   { ingoSet :: S.Set EdgeID
--   , outgoSet :: S.Set EdgeID
--   , ndLabel :: a }
--   deriving (Show, Eq, Ord)
--
--
-- -- | ID of an edge. The following properties must be satisfied by `EdgeID`:
-- --
-- --   * The ordering of edge IDs (`Ord` instance) is consistent with the
-- --     topological ordering of the edges.
-- --   * The smallest `EdgeID` of a given DAG, `minEdge`, is equal
-- --     to `0` (`EdgeID 0`).
-- --
-- -- Additional important property, which guarantees that inference computations
-- -- over the DAG, based on dynamic programming, are efficient:
-- --
-- --   * Let `e` be the greatest `EdgeID` in the DAG. Then, the set of `EdgeID`s
-- --     in the DAG is equal to {0 .. e}.
-- --
-- -- However, this last property is not required for the correcntess of the
-- -- inference computations, it only improves their memory complexity.
-- newtype EdgeID = EdgeID {unEdgeID :: Int}
--   deriving (Show, Eq, Ord, Num, A.Ix)
--
--
-- -- | Edge of the DAG.
-- data Edge a = Edge
--   { tailNode :: NodeID
--   , headNode :: NodeID
--   , edLabel  :: a }
--   deriving (Show, Eq, Ord, Functor, F.Foldable, T.Traversable)


-- ------------------------------------------------------------------
-- -- Primitive Operations
-- ------------------------------------------------------------------
--
--
-- -- | Return the tail node of the given edge.
-- begsWith :: EdgeID -> DAG a b -> NodeID
-- begsWith i DAG{..} = case M.lookup i edgeMap of
--   Nothing -> error "begsWith: incorrent edge ID"
--   Just Edge{..} -> tailNode
--
--
-- -- | Return the head node of the given edge.
-- endsWith :: EdgeID -> DAG a b -> NodeID
-- endsWith i DAG{..} = case M.lookup i edgeMap of
--   Nothing -> error "endsWith: incorrent edge ID"
--   Just Edge{..} -> headNode
--
--
-- -- | The list of outgoint edges from the given node, in ascending order.
-- incomingEdges :: NodeID -> DAG a b -> [EdgeID]
-- incomingEdges i DAG{..} = case M.lookup i nodeMap of
--   Nothing -> error "incomingEdges: incorrect ID"
--   Just Node{..} -> S.toAscList ingoSet
--
--
-- -- | The list of outgoint edges from the given node, in ascending order.
-- outgoingEdges :: NodeID -> DAG a b -> [EdgeID]
-- outgoingEdges i DAG{..} = case M.lookup i nodeMap of
--   Nothing -> error "outgoingEdges: incorrect ID"
--   Just Node{..} -> S.toAscList outgoSet
--
--
-- -- | The label assigned to the given node. Return `Nothing` if the node ID is
-- -- out of bounds.
-- maybeNodeLabel :: NodeID -> DAG a b -> Maybe a
-- maybeNodeLabel i DAG{..} = ndLabel <$> M.lookup i nodeMap
--
--
-- -- | The label assigned to the given node.
-- nodeLabel :: NodeID -> DAG a b -> a
-- nodeLabel i DAG{..} = case M.lookup i nodeMap of
--   Nothing -> error "nodeLabel: incorrect ID"
--   Just Node{..} -> ndLabel
--
--
-- -- | The label assigned to the given edge. Return `Nothing` if the edge ID is
-- -- out of bounds.
-- maybeEdgeLabel :: EdgeID -> DAG a b -> Maybe b
-- maybeEdgeLabel i DAG{..} = edLabel <$> M.lookup i edgeMap
--
--
-- -- | The label assigned to the given node.
-- edgeLabel :: EdgeID -> DAG a b -> b
-- edgeLabel i DAG{..} = case M.lookup i edgeMap of
--   Nothing -> error "edgeLabel: incorrent ID"
--   Just Edge{..} -> edLabel
--
--
-- -- | The greatest `EdgeID` in the DAG.
-- minEdge :: DAG a b -> EdgeID
-- minEdge = fst . M.findMin . edgeMap
--
--
-- -- | The greatest `EdgeID` in the DAG.
-- maxEdge :: DAG a b -> EdgeID
-- maxEdge = fst . M.findMax . edgeMap
--
--
-- ------------------------------------------------------------------
-- -- Not-so-primitive ops, but still looking at the implementation
-- ------------------------------------------------------------------
--
--
-- -- | The list of DAG nodes in ascending order.
-- dagNodes :: DAG a b -> [NodeID]
-- dagNodes = M.keys . nodeMap
--
--
-- -- | Map function over node labels.
-- mapN :: (a -> b) -> DAG a c -> DAG b c
-- mapN f dag =
--   dag {nodeMap = nodeMap'}
--   where
--     nodeMap' = M.fromList
--       [ (nodeID, node {ndLabel = newLabel})
--       | (nodeID, node) <- M.toList (nodeMap dag)
--       , let newLabel = f (ndLabel node) ]
--
--
-- -- | The list of DAG edges in ascending order.
-- dagEdges :: DAG a b -> [EdgeID]
-- dagEdges = M.keys . edgeMap
--
--
-- -- | Similar to `fmap` but the mapping function has access to IDs of the
-- -- individual edges.
-- mapE :: (EdgeID -> b -> c) -> DAG a b -> DAG a c
-- mapE f dag =
--   dag {edgeMap = edgeMap'}
--   where
--     edgeMap' = M.fromList
--       [ (edgeID, edge {edLabel = newLabel})
--       | (edgeID, edge) <- M.toList (edgeMap dag)
--       , let newLabel = f edgeID (edLabel edge) ]
--
--
-- -- | Zip labels assigned to the same edges in the two input DAGs. Node labels
-- -- from the first DAG are preserved. The function fails if the input DAGs
-- -- contain different sets of edge IDs or node IDs.
-- zipE :: DAG a b -> DAG x c -> DAG a (b, c)
-- zipE dagL dagR
--   | M.keysSet (nodeMap dagL) /= M.keysSet (nodeMap dagR) =
--       error "zipE: different sets of node IDs"
--   | M.keysSet (edgeMap dagL) /= M.keysSet (edgeMap dagR) =
--       error "zipE: different sets of edge IDs"
--   | otherwise = DAG
--       { nodeMap = newNodeMap
--       , edgeMap = newEdgeMap }
--   where
--     newNodeMap = nodeMap dagL
--     newEdgeMap = M.fromList
--       [ (edgeID, newEdge)
--       | edgeID <- M.keys (edgeMap dagL)
--       , let newEdge = mergeEdges
--               (edgeMap dagL M.! edgeID)
--               (edgeMap dagR M.! edgeID) ]
--     mergeEdges e1 e2
--       | tailNode e1 /= tailNode e2 =
--           error "zipE.mergEdges: different tail nodes"
--       | headNode e1 /= headNode e2 =
--           error "zipE.mergEdges: different head nodes"
--       | otherwise =
--           let newLabel = (edLabel e1, edLabel e2)
--           in  e1 {edLabel = newLabel}
--
--
-- ------------------------------------------------------------------
-- -- Intermediate Operations
-- ------------------------------------------------------------------
--
--
-- -- | The list of the preceding edges of the given edge.
-- prevEdges :: EdgeID -> DAG a b -> [EdgeID]
-- prevEdges edgeID dag =
--   let tailNodeID = begsWith edgeID dag
--   in  ingoingEdges tailNodeID dag
--
--
-- -- | Is the given edge initial?
-- isInitialEdge :: EdgeID -> DAG a b -> Bool
-- isInitialEdge edgeID = null . prevEdges edgeID
--
--
-- -- -- | Is the given node initial?
-- -- isInitialNode :: NodeID -> DAG a b -> Bool
-- -- isInitialNode nodeID = null . ingoingEdges nodeID
--
--
-- -- | The list of the succeding edges of the given edge.
-- nextEdges :: EdgeID -> DAG a b -> [EdgeID]
-- nextEdges edgeID dag =
--   let headNodeID = endsWith edgeID dag
--   in  outgoingEdges headNodeID dag
--
--
-- -- | Is the given edge initial?
-- isFinalEdge :: EdgeID -> DAG a b -> Bool
-- isFinalEdge edgeID = null . nextEdges edgeID
--
--
-- ------------------------------------------------------------------
-- -- Conversion: List
-- ------------------------------------------------------------------
--
--
-- -- | Convert a sequence of (node label, edge label) pairs to a trivial DAG.
-- -- The first argument is the first node label.
-- _fromList :: a -> [(a, b)] -> DAG a b
-- _fromList nodeLabel0 xs = DAG
--   { nodeMap = newNodeMap -- M.unions [begNodeMap, middleNodeMap, endNodeMap]
--   , edgeMap = newEdgeMap }
--   where
--
--     newNodeMap = M.fromList $ do
--       let nodeLabels = nodeLabel0 : map fst xs
--           xsLength = length xs
--       (i, y) <- zip [0 .. length xs] nodeLabels
--       let node = Node
--             { ingoSet  =
--                 if i > 0
--                 then S.singleton $ EdgeID (i-1)
--                 else S.empty
--             , outgoSet =
--                 if i < xsLength
--                 then S.singleton $ EdgeID i
--                 else S.empty
--             , ndLabel = y }
--       return (NodeID i, node)
--
--     newEdgeMap = M.fromList $ do
--       (i, x) <- zip [0..] (map snd xs)
--       let edge = Edge
--             { tailNode = NodeID i
--             , headNode = NodeID (i+1)
--             , edLabel  = x }
--       return (EdgeID i, edge)
--
--
-- -- | Convert a sequence of items to a trivial DAG. Afterwards, check if the
-- -- resulting DAG is well-structured and throw error if not.
-- fromList :: [a] -> DAG () a
-- fromList xs =
--   if isOK dag
--   then dag
--   else error "fromList: resulting DAG not `isOK`"
--   where
--     dag = _fromList () $ zip (repeat ()) xs
--
--
-- -- | Convert a sequence of items to a trivial DAG. Afterwards, check if the
-- -- resulting DAG is well-structured and throw error if not.
-- fromList' :: a -> [(a, b)] -> DAG a b
-- fromList' x xs =
--   if isOK dag
--   then dag
--   else error "fromList': resulting DAG not `isOK`"
--   where
--     dag = _fromList x xs
--
--
-- ------------------------------------------------------------------
-- -- Conversion: DAG
-- ------------------------------------------------------------------
--
--
-- -- | Convert a sequence of labeled edges into a dag.
-- -- The function assumes that edges are given in topological order.
-- _fromEdgesUnsafe :: [Edge a] -> DAG () a
-- _fromEdgesUnsafe edges = DAG
--   { nodeMap = newNodeMap
--   , edgeMap = newEdgeMap }
--   where
--
--     newEdgeMap = M.fromList $ do
--       (i, edge) <- zip [0..] edges
--       return (EdgeID i, edge)
--
--     tailMap = M.fromListWith S.union $ do
--       (i, edge) <- zip [0..] edges
--       return (tailNode edge, S.singleton $ EdgeID i)
--
--     headMap = M.fromListWith S.union $ do
--       (i, edge) <- zip [0..] edges
--       return (headNode edge, S.singleton $ EdgeID i)
--
--     newNodeMap = M.fromList $ do
--       nodeID <- S.toList $ S.union (M.keysSet headMap) (M.keysSet tailMap)
--       let ingo = case M.lookup nodeID headMap of
--             Nothing -> S.empty
--             Just st -> st
--           ougo = case M.lookup nodeID tailMap of
--             Nothing -> S.empty
--             Just st -> st
--           node = Node
--             { ingoSet = ingo
--             , outgoSet = ougo
--             , ndLabel = () }
--       return (nodeID, node)
--
--
-- -- | Convert a sequence of labeled edges into a dag.
-- -- The function assumes that edges are given in topological order.
-- fromEdgesUnsafe :: [Edge a] -> DAG () a
-- fromEdgesUnsafe xs =
--   if isOK dag
--   then dag
--   else error "fromEdgesUnsafe: resulting DAG not `isOK`"
--   where
--     dag = _fromEdgesUnsafe xs
--
--
-- -- ------------------------------------------------------------------
-- -- -- Provisional
-- -- ------------------------------------------------------------------
-- --
-- --
-- -- -- | Convert the DAG to a list, provided that it was constructed from a list,
-- -- -- which is not checked.
-- -- toListProv :: DAG () a -> [a]
-- -- toListProv DAG{..} =
-- --   [ edLabel edge
-- --   | (_edgeID, edge) <- M.toAscList edgeMap ]
--
--
-- ------------------------------------------------------------------
-- -- Check
-- ------------------------------------------------------------------
--
--
-- -- | Check if the DAG is well-structured.
-- isOK :: DAG a b -> Bool
-- isOK DAG{..} =
--   nodeMapOK && edgeMapOK
--   where
--     nodeMapOK = and
--       [ M.member edgeID edgeMap
--       | (_nodeID, Node{..}) <- M.toList nodeMap
--       , edgeID <- S.toList (S.union ingoSet outgoSet) ]
--     edgeMapOK = and
--       [ M.member nodeID nodeMap
--       | (_edgeID, Edge{..}) <- M.toList edgeMap
--       , nodeID <- [tailNode, headNode] ]
