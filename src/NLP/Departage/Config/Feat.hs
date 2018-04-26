{-# LANGUAGE DeriveGeneric #-}


module NLP.Departage.Config.Feat
  ( FeatConfig(..)
  , UnaryOption(..)
  , ParentOption(..)
  , SisterOption(..)
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
  | ParentLemmaParent
  | ParentLemmaCurrent
  | ParentTagsOnly
  | ParentTagsAndDepRel
  | ParentUnordLemma
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
