{-# LANGUAGE DeriveGeneric #-}


module NLP.Traversal.Config
  ( Config(..)
  , NewConfig(..)
  ) where


-- import qualified Data.Map.Strict as M

import           Dhall
import qualified Data.Aeson as JSON

import qualified NLP.Traversal.Config.Feat as Feat


-- | Annotation configuration.
data Config = Config
  { baseFeatConfig :: Feat.FeatConfig
  , mweFeatConfig :: Feat.FeatConfig

  -- TODO: Problematic when simple models are used -- each model do the lifting,
  -- while it should be done only once! Probably even with the ensemble model
  -- this is wrong!
  -- , liftCase :: Bool

  , sequential :: Bool
    -- ^ Use sequential encoding (e.g. to make the tagger robust to dependency
    -- parsing errors).
  } deriving (Generic, Show)

instance Interpret Config

instance JSON.FromJSON Config
instance JSON.ToJSON Config where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions


----------------------------------------------
-- Tagging
----------------------------------------------


-- | Annotation configuration.
data NewConfig = NewConfig
  { featConfig :: Feat.IOBFeatConfig
  } deriving (Generic, Show)

instance Interpret NewConfig

instance JSON.FromJSON NewConfig
instance JSON.ToJSON NewConfig where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
