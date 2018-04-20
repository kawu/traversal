{-# LANGUAGE RecordWildCards #-}


module NLP.Departage.Model
  ( Model (..)
  , save
  , load
  ) where


import           System.FilePath ((</>))

import qualified Data.Set as S

import qualified NLP.Departage.CRF.Map as Map
import qualified NLP.Departage.SharedTask as Task
import qualified NLP.Departage.DepTree.Cupt as Cupt


data Model = Model
  { paraMapBase :: Task.ParaMap Task.MWE
  , paraMapMwe :: Task.ParaMap (Maybe Cupt.MweTyp)
  , mweTypSet :: S.Set Cupt.MweTyp
  }


basePath :: FilePath
basePath = "base.model"


mwePath :: FilePath
mwePath = "mwe.model"


typPath :: FilePath
typPath = "typ.list"


-- | Save model in the given directory.
save :: Model -> FilePath -> IO ()
save Model{..} dirPath = do
  Map.save keyEnc paraMapBase (dirPath </> basePath)
  Map.save keyEnc paraMapMwe  (dirPath </> mwePath)
  writeFile (dirPath </> typPath) (show mweTypSet)


-- | Load model from the given directory.
load :: FilePath -> IO Model
load dirPath = Model
  <$> Map.load keyEnc (dirPath </> basePath)
  <*> Map.load keyEnc (dirPath </> mwePath)
  <*> (read <$> readFile (dirPath </> typPath))


keyEnc :: (Show a, Read a) => Map.Encoding a
keyEnc = Map.Encoding
  { Map.toStr = show
  , Map.fromStr = read
  }
