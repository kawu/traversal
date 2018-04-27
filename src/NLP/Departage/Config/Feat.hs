{-# LANGUAGE DeriveGeneric #-}


module NLP.Departage.Config.Feat
  ( FeatConfig(..)
  , UnaryOption(..)
  , ParentOption(..)
  , SisterOption(..)
  , GrandpaOption(..)
  , Set(..)
  ) where


import qualified Data.Set as S
import qualified Data.Vector as V

import           Dhall
import qualified Data.Aeson as JSON


-- | Feature configuration.
data FeatConfig = FeatConfig
  { unaryOptions :: Set UnaryOption
  , parentOptions :: Set ParentOption
  , sisterOptions :: Set SisterOption
  , grandpaOptions :: Set GrandpaOption
  } deriving (Generic, Show, Eq, Ord)


instance Interpret FeatConfig

instance JSON.FromJSON FeatConfig
instance JSON.ToJSON FeatConfig where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions


-- | Types of unary options.
data UnaryOption
  = UnaryOrth
    -- ^ Standard unary (orth, mwe) pair
  | UnaryLemma
    -- ^ (lemma, mwe)
  | UnaryPos
    -- ^ (POS, mwe)
  | UnaryDepRel
    -- ^ (parent dependency relation, mwe)
  deriving (Generic, Read, Show, Eq, Ord)

instance Interpret UnaryOption

instance JSON.FromJSON UnaryOption
instance JSON.ToJSON UnaryOption where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions


-- | Types of binary parent/child options.
data ParentOption
  = ParentOrth
    -- ^ Standard (orth, mwe) pair for parent and child
  | ParentLemma
    -- ^ MWE tags and lemmas of the node and its parent
  | ParentLemmaParent
    -- ^ MWE tags of the node and its parent, lemma of the parent
  | ParentLemmaCurrent
    -- ^ MWE tags of the node and its parent, lemma of the node
  | ParentLemmaParentPosCurrentDepRel
    -- ^ MWE tags of the node and its parent;
    -- lemma of the parent;
    -- pos of the node (child);
    -- dependency relation between the two
  | ParentLemmaCurrentPosParentDepRel
    -- ^ MWE tags of the node and its parent;
    -- lemma of the node;
    -- pos of the parent;
    -- dependency relation between the two
  | ParentTagsOnly
    -- ^ MWE tags of the node and its parent
  | ParentTagsAndDepRel
    -- ^ MWE tags of the node and its grandpa + deprel between the two
  | ParentUnordLemma
    -- ^ MWE tags and lemmas of the node and its parent, unordered
  deriving (Generic, Read, Show, Eq, Ord)

instance Interpret ParentOption

instance JSON.FromJSON ParentOption
instance JSON.ToJSON ParentOption where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions


-- | Types of binary sister/sister options.
data SisterOption
  = SisterOrth
    -- ^ Standard (orth, mwe) pair for sisters
  | SisterLemma
  | SisterLemmaSisterPosCurrent
    -- ^ MWE tags of the node and its sister;
    -- lemma of the sister;
    -- pos of the node;
  | SisterLemmaCurrentPosSister
    -- ^ MWE tags of the node and its sister;
    -- lemma of the node;
    -- pos of the sister;
  | SisterLemmaSister
  | SisterLemmaCurrent
  | SisterTagsOnly
  | SisterTagsAndDepRel
  | SisterUnordLemma
  deriving (Generic, Read, Show, Eq, Ord)


instance Interpret SisterOption

instance JSON.FromJSON SisterOption
instance JSON.ToJSON SisterOption where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions


-- | Types of binary granpa/child options.
data GrandpaOption
  = GrandpaOrth
    -- ^ MWE tags and orthographic forms of both the node and its grandpa
  | GrandpaLemma
    -- ^ MWE tags and lemmas of the node and its grandpa
  | GrandpaTagsOnly
    -- ^ MWE tags of the node and its grandpa
  | GrandpaTagsAndDepRel
    -- ^ MWE tags of the node and its grandpa + deprel of the node (but not its
    -- parent! TODO: change this?)
  | GrandpaUnordLemma
    -- ^ MWE tags and lemmas of the node and its grandpa, unordered
    -- TODO: probably not a good idea to use it, since it mixes adjacent with
    -- non-adjacent lemma pairs.
  deriving (Generic, Read, Show, Eq, Ord)

instance Interpret GrandpaOption

instance JSON.FromJSON GrandpaOption
instance JSON.ToJSON GrandpaOption where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions


------------------------------
-- Set
------------------------------


-- data MapPair a = MapPair
--   { key :: Text
--   , val :: a
--   } deriving (Generic, Show)
--
-- instance Interpret a => Interpret (MapPair a)
--
-- toPair :: MapPair a -> (Text, a)
-- toPair MapPair{..} = (key, val)


newtype Set a = Set {unSet :: S.Set a}
  deriving (Generic, Show, Eq, Ord)

instance (Ord a, Interpret a) => Interpret (Set a) where
    autoWith = fmap
      (fmap $ Set . S.fromList . V.toList)
      autoWith

instance (Ord a, JSON.FromJSON a) => JSON.FromJSON (Set a)
instance JSON.ToJSON a => JSON.ToJSON (Set a) where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
