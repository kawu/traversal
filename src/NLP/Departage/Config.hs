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
--   , oneModelPerMweTyp :: Bool
--     -- ^ Train separate models for the individual MWE types. This may be
--     -- reasonable because in the shared task words can be annotated with many
--     -- MWEs at the same time.
  } deriving (Generic, Show)

instance Interpret Config

instance JSON.FromJSON Config
instance JSON.ToJSON Config where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
