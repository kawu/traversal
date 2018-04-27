{-# LANGUAGE DeriveGeneric #-}


module NLP.Departage.Config
  ( Config(..)
  ) where


-- import qualified Data.Map.Strict as M

import           Dhall
import qualified Data.Aeson as JSON

import qualified NLP.Departage.Config.Feat as Feat


-- | Annotation configuration.
data Config = Config
  { baseFeatConfig :: Feat.FeatConfig
  , mweFeatConfig :: Feat.FeatConfig
  , liftCase :: Bool
  , sequential :: Bool
    -- ^ Use sequential encoding (e.g. to make the tagger robust to dependency
    -- parsing errors).
  } deriving (Generic, Show)

instance Interpret Config

instance JSON.FromJSON Config
instance JSON.ToJSON Config where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
