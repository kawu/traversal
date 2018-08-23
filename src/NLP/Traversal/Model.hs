{-# LANGUAGE RecordWildCards #-}


module NLP.Traversal.Model
  ( EnsembleModel (..)
  , saveEnsemble
  , loadEnsemble

  , SimpleModel (..)
  , saveSimple
  , loadSimple

  , IOBModel (..)
  , saveIOB
  , loadIOB
  ) where


import           System.FilePath ((</>))
import           Control.Monad (forM_)

import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import qualified NLP.Traversal.CRF.Map as Map
import qualified NLP.Traversal.Core as Core
import qualified NLP.Traversal.DepTree.Cupt as Cupt


----------------------------------------------
-- Core
----------------------------------------------


typPath :: FilePath
typPath = "typ.list"


----------------------------------------------
-- Ensemble
----------------------------------------------


data EnsembleModel = EnsembleModel
  { paraMapBase :: Core.ParaMap Core.MWE
  , paraMapMwe :: Core.ParaMap (Maybe Cupt.MweTyp)
  , mweTypSet :: S.Set Cupt.MweTyp
  }


basePath :: FilePath
basePath = "base.model"


mwePath :: FilePath
mwePath = "mwe.model"


-- | Save model in the given directory.
saveEnsemble :: EnsembleModel -> FilePath -> IO ()
saveEnsemble EnsembleModel{..} dirPath = do
  Map.save keyEnc paraMapBase (dirPath </> basePath)
  Map.save keyEnc paraMapMwe  (dirPath </> mwePath)
  writeFile (dirPath </> typPath) (show mweTypSet)


-- | Load model from the given directory.
loadEnsemble :: FilePath -> IO EnsembleModel
loadEnsemble dirPath = EnsembleModel
  <$> Map.load keyEnc (dirPath </> basePath)
  <*> Map.load keyEnc (dirPath </> mwePath)
  <*> (read <$> readFile (dirPath </> typPath))


----------------------------------------------
-- Separate
----------------------------------------------


type SimpleModel = Core.ParaMap Core.MWE


-- elemPath :: T.Text -> FilePath
-- elemPath elem = T.unpack elem ++ ".model"


-- | Save simple model in the given file.
saveSimple :: SimpleModel -> FilePath -> IO ()
saveSimple paraMap filePath = Map.save keyEnc paraMap filePath


-- | Load simple model from the given directory.
loadSimple :: FilePath -> IO SimpleModel
loadSimple filePath = Map.load keyEnc filePath


-- -- | Load model from the given directory.
-- loadEnsemble :: FilePath -> IO EnsembleModel
-- loadEnsemble dirPath = EnsembleModel
--   <$> Map.load keyEnc (dirPath </> basePath)
--   <*> Map.load keyEnc (dirPath </> mwePath)
--   <*> (read <$> readFile (dirPath </> typPath))


----------------------------------------------
-- IOB
----------------------------------------------


type IOBModel = Core.ParaMap Core.IOB


-- | Save IOB model in the given file.
saveIOB :: IOBModel -> FilePath -> IO ()
saveIOB paraMap filePath = Map.save keyEnc paraMap filePath


-- | Load IOB model from the given directory.
loadIOB :: FilePath -> IO IOBModel
loadIOB filePath = Map.load keyEnc filePath


----------------------------------------------
-- Utils
----------------------------------------------


keyEnc :: (Show a, Read a) => Map.Encoding a
keyEnc = Map.Encoding
  { Map.toStr = show
  , Map.fromStr = read
  }
