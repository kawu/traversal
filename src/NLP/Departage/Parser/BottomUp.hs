{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}


-- | A simple bottom-up dependency parser.


module NLP.Departage.Parser.BottomUp
(
-- * TODO
  chartParse

) where


import           GHC.Generics (Generic)
import           Prelude hiding (span)

import           Data.Lens.Light
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.MemoTrie as Memo
import           Data.MemoTrie ((:->:))

import qualified Pipes as Pipes

import qualified NLP.Departage.Hype as H


--------------------------------------------------
-- Labeled hypergraph
--------------------------------------------------


-- | A hypergraph with labeled edges/nodes(?)
data LabHype a = LabHype
  { rawHype :: H.Hype
  , nodeLabel :: M.Map H.Node a
  }


--------------------------------------------------
-- Base types for chart parser
--------------------------------------------------


-- | A position in the input sentence.
type Pos = Int


-- | A span over the input sentence.
data Span = Span {
    -- | The starting position.
      _beg   :: {-# UNPACK #-} !Pos
    -- | The ending position (or rather the position of the dot).
    , _end   :: {-# UNPACK #-} !Pos
    } deriving (Show, Eq, Ord, Generic)
$( makeLenses [''Span] )


instance Memo.HasTrie Span where
  newtype (Span :->: b) = SpanTrie { unSpanTrie :: Memo.Reg Span :->: b }
  trie = Memo.trieGeneric SpanTrie
  untrie = Memo.untrieGeneric unSpanTrie
  enumerate = Memo.enumerateGeneric unSpanTrie


-- | A chart item parametrized by the type of lexical objects stored inside.
data Item a = Item
  { _span  :: {-# UNPACK #-} !Span
  , _value :: a
  } deriving (Show, Eq, Ord, Generic)
$( makeLenses [''Item] )


instance Memo.HasTrie a => Memo.HasTrie (Item a) where
  newtype (Item a :->: b) = ItemTrie { unItemTrie :: Memo.Reg (Item a) :->: b }
  trie = Memo.trieGeneric ItemTrie
  untrie = Memo.untrieGeneric unItemTrie
  enumerate = Memo.enumerateGeneric unItemTrie


--------------------------------------------------
-- TODO
--------------------------------------------------


-- | Node typ
data Typ
  = L
    -- ^ Head on the left
  | R
    -- ^ Head on the right
  | B
    -- ^ Heads on both sides
  deriving (Show, Eq, Ord)


-- -- | Creates the hypergraph representing the space of all dependency parses over
-- -- the given sentence.
-- chartParse :: [a] -> H.Hype
-- chartParse sent
--
--   = H.fromList
--   . M.toList
--   $ M.fromListWith M.union
--   [ (nodeID, M.singleton arcID nodeSet)
--   | (node, nodeID) <- M.toList nodeMap
--   , nodeSet <- incoming node
--   , let arcID = arc nodeSet ]
--
--   where
--
--     -- length of the sentence
--     n = length sent
--
--     -- node identifiers
--     node typ i j = nodeMap M.! (i, j, typ)
--     nodeMap = M.fromList $ zip
--       [ (i, j, typ)
--       | i <- [0 .. n]
--       , j <- [i .. n]
--       , typ <- [L, R, B]
--       , (typ == B) `implies` (j > i)
--       ] (map H.Node [0..])
--
--     -- Both (i < j)
--     both i j = do
--       k <- [i .. j-1]
--       return [node L i k, node R (k+1) j]
--
--     -- Left (i <= j)
--     left i j
--       | i == j = []
--       | otherwise = do
--           k <- [i+1 .. j]
--           return [node B i k, node L k j]
--
--     -- Right (i <= j)
--     right i j
--       | i == j = []
--       | otherwise = do
--           k <- [i .. j-1]
--           return [node R i k, node B k j]
--
--     incoming (i, j, typ) = map S.fromList $
--       case typ of
--         B -> both i j
--         L -> left i j
--         R -> right i j
--
--     arc nodeSet = arcMap M.! nodeSet
--     arcMap = M.fromList $ zip
--       [ nodeSet
--       | (node, nodeID) <- M.toList nodeMap
--       , nodeSet <- incoming node
--       ] (map H.Arc [0..])
--
--
--     -- each = Pipes.Select . Pipes.each


-- | Creates the hypergraph representing the space of all dependency parses over
-- the given sentence.
--
-- TODO: Seems to only work for sentences of length > 1.
chartParse :: [a] -> H.Hype
chartParse sent

  = H.fromList
  [ (nodeID node, process arcSet)
  | (node, arcSet) <- M.toList arcMap ]

  where

    process arcSet = M.fromList
      [ ( arcID arc
        , S.fromList . map nodeID . S.toList $ arc )
      | arc <- S.toList arcSet ]

    nodeID = (M.!) nodeIDMap
    nodeSet = nodes sent
    nodeIDMap = M.fromList $ zip
      (S.toList nodeSet)
      (map H.Node [0..])

    arcID = (M.!) arcIDMap
    arcMap = arcs nodeSet
    arcIDMap = M.fromList $ zip
      [ arc
      | arcSet <- M.elems arcMap
      , arc <- S.toList arcSet ]
      (map H.Arc [0..])


type Node = (Pos, Pos, Typ)
type Arc = S.Set Node


-- | Retrieve the hypernodes for a given sentence.
nodes :: [a] -> S.Set Node
nodes sent = S.fromList
  [ (i, j, typ)
  | i <- [0 .. n-1]
  , j <- [i .. n-1]
  , typ <- [L, R, B]
  , (typ == B) `implies` (j > i)
  ]
  where
    n = length sent


-- | Retrieve the hyperarcs for a given sentence.
arcs :: S.Set Node -> M.Map Node (S.Set Arc)
arcs nodeSet =

  M.fromList
  [ (nd, incoming nd)
  | nd <- S.toList nodeSet ]

  where

    -- A node (TODO: don't think this is useful)
    node typ i j = (i, j, typ)

    -- Both (i < j)
    both i j = do
      k <- [i .. j-1]
      return [node L i k, node R (k+1) j]

    -- Left (i <= j)
    left i j
      | i == j = []
      | otherwise = do
          k <- [i+1 .. j]
          return [node B i k, node L k j]

    -- Right (i <= j)
    right i j
      | i == j = []
      | otherwise = do
          k <- [i .. j-1]
          return [node R i k, node B k j]

    incoming (i, j, typ) = S.fromList . map S.fromList $
      case typ of
        B -> both i j
        L -> left i j
        R -> right i j


implies :: Bool -> Bool -> Bool
implies p q = if p then q else True
