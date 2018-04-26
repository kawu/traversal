{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}


-- | A simple bottom-up dependency parser.


module NLP.Departage.Parser.BottomUp
(
  -- * Types
    Typ (..)
  , Pos
  , Item
  , ParHype (..)
  -- , Tail

  -- * Hypergraph
  , mkHype
  , printHype
  , printDeps

  -- * Unfolding
  , unfold

  -- * Convertion
  , toDepTree
) where


-- import           GHC.Generics (Generic)
-- import           Prelude hiding (span)
import           Control.Monad (forM_)

-- import           Data.Maybe (maybeToList)
-- import           Data.Lens.Light
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Tree as R
import           Data.Ord (comparing)
-- import qualified Data.MemoTrie as Memo
-- import           Data.MemoTrie ((:->:))

-- import qualified Pipes as Pipes

import qualified NLP.Departage.Hype as H
import qualified NLP.Departage.DepTree as Dep

-- import Debug.Trace (trace)


--------------------------------------------------
-- Base types
--------------------------------------------------


-- | A position in the input sentence
type Pos = Int


-- | Node/item type
data Typ
  = L
    -- ^ Head on the left (equivalently, left node is active, i.e., it still
    -- needs a parent)
  | R
    -- ^ Head on the right
  | B
    -- ^ Heads on both sides
  | T Pos
    -- ^ Final (top) hypergraph node with information about sentence's root word
  deriving (Show, Eq, Ord)


-- | An item/node in the parsing chart
type Item = (Pos, Pos, Typ)


--------------------------------------------------
-- Parsing hypergraph
--------------------------------------------------


-- | Parsing hypergraph
data ParHype = ParHype
  { rawHype :: H.Hype
  , nodeLabel :: M.Map H.Node Item
  }


-- | Print the given labeled hypergraph
printHype :: ParHype -> IO ()
printHype ParHype{..} = do
  forM_ (S.toList $ H.nodes rawHype) $ \nodeID -> do
    putStr $ "[" ++ show (H.unNode nodeID) ++ "] "
    putStrLn $ show (nodeLabel M.! nodeID)
    forM_ (H.incoming nodeID rawHype) $ \arcID -> do
      putStr $ "  <" ++ show (H.unArc arcID) ++ "> "
      let tailSet = H.tail arcID rawHype
      putStrLn $ L.intercalate " + "
        [ show (nodeLabel M.! nodeID')
        | nodeID'  <- S.toList tailSet ]


-- | Print the dependency trees encoded in the graph.
printDeps :: ParHype -> IO ()
printDeps hype = do
  forM_ (unfold hype) $ \path -> do
    let dep = toDepTree path
    putStrLn . R.drawTree . fmap (show . fst) $ Dep.toRose dep


--------------------------------------------------
-- Hyperpaths
--------------------------------------------------


-- | A hyperpath from the parsing hypergraph.
type Path = R.Tree Item


-- | Convert the path to a dependency tree.
toDepTree
  :: Path -- ^ The path
  -> Dep.Tree Pos ()
toDepTree =

  top

  where

    top (R.Node (_, _, T _) [l, r]) =
      join (right l) (left r)
    top _ = error "top"

    both (R.Node (_, _, B) [l, r]) =
      (left l, right r)
    both _ = error "both"

    left (R.Node (i, j, L) [])
      | i == j = Dep.Tree i []
      | otherwise = error "left init"
    left (R.Node (_, _, L) [l, r]) =
      let (llTree, lrTree) = both l
          rTree = left r
      in  leftUp llTree (join lrTree rTree)
    left item = error $ "left: " ++ show item

    right (R.Node (i, j, R) [])
      | i == j = Dep.Tree i []
      | otherwise = error "right init"
    right (R.Node (_, _, R) [l, r]) =
      let lTree = right l
          (rlTree, rrTree) = both r
      in  rightUp (join lTree rlTree) rrTree
    right _ = error "right"

    -- | Merge two corresponding trees (left and right, respectively)
    join :: Dep.Tree Pos () -> Dep.Tree Pos () -> Dep.Tree Pos ()
    join l r
      | Dep.root l == Dep.root r =
          Dep.Tree
          { root = Dep.root l
          , children = Dep.children l ++ Dep.children r }
      | otherwise = error "join"

    -- | Make the left tree the parent of the right tree
    leftUp :: Dep.Tree Pos () -> Dep.Tree Pos () -> Dep.Tree Pos ()
    leftUp l r = l {Dep.children = Dep.children l ++ [(r, ())]}

    -- | Make the right tree the parent of the left tree
    rightUp :: Dep.Tree Pos () -> Dep.Tree Pos () -> Dep.Tree Pos ()
    rightUp l r = r {Dep.children = (l, ()) : Dep.children r}


--------------------------------------------------
-- Hypergraph unfolding
--------------------------------------------------


-- | Retrieve the individual paths encoded in the hypergraph.
unfold :: ParHype -> [Path]
unfold ParHype{..} =

  concat
  [ unfo nodeID
  | nodeID <- S.toList $ H.final rawHype ]

  where

    -- nodeMap = revMap nodeLabel

    unfo nodeID
      | S.null incoming =
          return $ R.Node (nodeLabel M.! nodeID) []
      | otherwise = do
          arcID <- S.toList incoming
          children <- mapM unfo . L.sortBy (comparing itemRepr) . S.toList $
            H.tail arcID rawHype
          return $ R.Node (nodeLabel M.! nodeID) children
      where
        incoming = H.incoming nodeID rawHype
        itemRepr = (M.!) nodeLabel



--------------------------------------------------
-- Parsing hypergraph
--------------------------------------------------


-- | Creates the hypergraph representing the space of all dependency parses over
-- the given sentence.
--
-- TODO: Seems to only work for sentences of length > 1.
mkHype :: [a] -> ParHype
mkHype sent =

  ParHype
  { rawHype = raw
  , nodeLabel = revMap nodeIDMap
  }

  where

    raw = H.fromList
      [ (nodeID node, process arcSet)
      | (node, arcSet) <- M.toList arcMap ]

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


--------------------------------------------------
-- Chart parsing
--------------------------------------------------


-- | Retrieve the hypernodes for a given sentence.
nodes :: [a] -> S.Set Item
nodes sent = S.fromList $
  [ (i, j, typ)
  | i <- [0 .. n-1]
  , j <- [i .. n-1]
  , typ <- [L, R, B]
  , (typ == B) `implies` (j > i)
  ] ++
  [ (0, n-1, T i)
  | i <- [0 .. n - 1]
  ]
  where
    n = length sent


-- | Tail of a given hyperarc (which explicates from which items a given item
-- can be constructed)
type Tail = S.Set Item


-- | Retrieve the hyperarcs for a given sentence.
arcs :: S.Set Item -> M.Map Item (S.Set Tail)
arcs nodeSet =

  M.fromList
  [ (nd, incoming nd)
  | nd <- S.toList nodeSet ]

  where

    incoming (i, j, typ) = S.fromList . map S.fromList $
      case typ of
        B -> both i j
        L -> left i j
        R -> right i j
        T h -> top i j h

    -- Both (i < j)
    both i j = do
      k <- [i .. j-1]
      -- return [node L i k, node R (k+1) j]
      return [(i, k, L), (k+1, j, R)]

    -- Left (i <= j)
    left i j
      | i == j = []
      | otherwise = do
          k <- [i+1 .. j]
          -- return [node B i k, node L k j]
          return [(i, k, B), (k, j, L)]

    -- Right (i <= j)
    right i j
      | i == j = []
      | otherwise = do
          k <- [i .. j-1]
          -- return [node R i k, node B k j]
          return [(i, k, R), (k, j, B)]

    -- Top (i <= j)
    top i j h =
      return [(i, h, R), (h, j, L)]

    -- -- A node (TODO: don't think this is useful)
    -- node typ i j = (i, j, typ)



--------------------------------------------------
-- Utils
--------------------------------------------------


implies :: Bool -> Bool -> Bool
implies p q = if p then q else True


revMap :: (Ord b) => M.Map a b -> M.Map b a
revMap = M.fromList . map (\(x, y) -> (y, x)) . M.toList
