{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


import           Control.Monad (forM_)
import           Data.Monoid ((<>))
import           Options.Applicative
-- import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing)
import           Data.String (fromString)
import           Data.List (sortBy)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL

import           System.FilePath (isAbsolute, (</>))

import qualified Dhall as Dhall

import qualified NLP.Departage.CRF.SGD as SGD
-- import qualified NLP.Departage.CRF.Map as Map
import qualified NLP.Departage.DepTree.Cupt as Cupt
import qualified NLP.Departage.SharedTask as Task
-- import qualified NLP.Departage.FeatConfig as Cfg
import qualified NLP.Departage.Config as Cfg
import qualified NLP.Departage.Model as Model


--------------------------------------------------
-- Commands
--------------------------------------------------


data Command
    = Train
      { configPath :: FilePath
        -- ^ Feature configuration path
      , trainPath :: FilePath
        -- ^ Train dataset
      , devPath :: Maybe FilePath
        -- ^ Dev dataset (optional)
      , modelPathMay :: Maybe FilePath
        -- ^ Directory/file path to store the model (optional)
      , mweType :: Maybe T.Text
        -- ^ Focus on a single MWE type and create a "simple" model
      }
    | Tag
      { configPath :: FilePath
        -- ^ Feature configuration path
      , modelPath :: FilePath
        -- ^ Model directory
--       , inpPath :: FilePath
--         -- ^ Dataset to tag
--       , outPath :: FilePath
--         -- ^ Output file
      , bestPath :: Bool
        -- ^ Use best path instead of marginals
      , mweType :: Maybe T.Text
        -- ^ Load a simple model and focus on a single MWE type
      }
    | LiftCase
    | RemoveDeriv
    | CopyLemma
    | Clear
    | DepStats


--------------------------------------------------
-- Parse options
--------------------------------------------------


trainOptions :: Parser Command
trainOptions = Train
  <$> strOption
        ( metavar "CONFIG"
       <> long "config"
       <> short 'c'
       <> help "Configuration file"
        )
  <*> strOption
        ( metavar "TRAIN"
       <> long "train"
       <> short 't'
       <> help "Training dataset"
        )
  <*> (optional . strOption)
        ( metavar "DEV"
       <> long "dev"
       <> short 'd'
       <> help "Development dataset"
        )
  <*> (optional . strOption)
        ( metavar "OUTPUT-MODEL-DIR"
       <> long "model"
       <> short 'm'
       <> help "Output model directory"
        )
  <*> (optional . strOption)
        ( metavar "MWE-TYPE"
       <> long "mwe"
       <> help "Focus on a particular MWE type"
        )


tagOptions :: Parser Command
tagOptions = Tag
  <$> strOption
        ( metavar "CONFIG"
       <> long "config"
       <> short 'c'
       <> help "Configuration file"
        )
  <*> strOption
        ( metavar "MODEL"
       <> long "model"
       <> short 'm'
       <> help "Model directory/file"
        )
--   <*> strOption
--         ( metavar "INPUT"
--        <> long "input"
--        <> short 'i'
--        <> help "Input file"
--         )
--   <*> strOption
--         ( metavar "OUTPUT"
--        <> long "output"
--        <> short 'o'
--        <> help "Output file"
--         )
  <*> switch
        ( long "bestpath"
       <> short 'b'
       <> help "Use best path instead of marginals"
        )
  <*> (optional . strOption)
        ( metavar "MWE-TYPE"
       <> long "mwe"
       <> help "Focus on a particular MWE type"
        )


liftCaseOptions :: Parser Command
liftCaseOptions = pure LiftCase


removeDerivOptions :: Parser Command
removeDerivOptions = pure RemoveDeriv


copyLemmaOptions :: Parser Command
copyLemmaOptions = pure CopyLemma


clearOptions :: Parser Command
clearOptions = pure Clear


depStatsOptions :: Parser Command
depStatsOptions = pure DepStats


--------------------------------------------------
-- Global options
--------------------------------------------------


opts :: Parser Command
opts = subparser
  ( command "train"
    (info (helper <*> trainOptions)
      (progDesc "Train a model")
    )
    <> command "tag"
    (info (helper <*> tagOptions)
      (progDesc "Tag the input file")
    )
    <> command "liftcase"
    (info (helper <*> liftCaseOptions)
      (progDesc "Lift case markers (stdin -> stdout)")
    )
    <> command "removederiv"
    (info (helper <*> removeDerivOptions)
      (progDesc "Remove DERIVs (stdin -> stdout)")
    )
    <> command "copylemma"
    (info (helper <*> copyLemmaOptions)
      (progDesc "Copy lemma from orth where not present (stdin -> stdout)")
    )
    <> command "clear"
    (info (helper <*> clearOptions)
      (progDesc "Clear MWE annotations")
    )
    <> command "depstats"
    (info (helper <*> depStatsOptions)
      (progDesc "Show dependency relation statistics")
    )
  )


--------------------------------------------------
-- Main
--------------------------------------------------


-- | Run program depending on the cmdline arguments.
run :: Command -> IO ()
run cmd =

  case cmd of

    Tag{..} -> do
      let configPath' =
            if isAbsolute configPath
            then configPath
            else "./" </> configPath
          tagConfig = Task.TagConfig
            { tagBestPath = bestPath }
      config <- Dhall.detailed (Dhall.input Dhall.auto $ fromString configPath')
      case mweType of
        Nothing -> do
          model <- Model.loadEnsemble modelPath
          putStr "# MWE types: " >> print (Model.mweTypSet model)
          Task.tagEnsemble config tagConfig model -- inpPath outPath
        Just mweTyp -> do
          model <- Model.loadSimple modelPath
          Task.tagSimple config tagConfig mweTyp model -- inpPath outPath

    Train{..} -> do
      let configPath' =
            if isAbsolute configPath
            then configPath
            else "./" </> configPath
      config <- Dhall.detailed (Dhall.input Dhall.auto $ fromString configPath')
      case mweType of
        Nothing -> do
          model <- Task.trainEnsemble config trainPath devPath
          case modelPathMay of
            Just path -> Model.saveEnsemble model path
            Nothing -> return ()
        Just mweTyp -> do
          model <- Task.trainSimple config mweTyp trainPath devPath
          case modelPathMay of
            Just path -> Model.saveSimple model path
            Nothing -> return ()

--     Train{..} -> do
--       -- featCfg <- Cfg.loadConfig configPath
--       let configPath' =
--             if isAbsolute configPath
--             then configPath
--             else "./" </> configPath
--       config <- Dhall.detailed (Dhall.input Dhall.auto $ fromString configPath')
--
--       putStrLn "# Basic model"
--       -- let readData = fmap (mapMaybe $ Task.encodeCupt . Cupt.decorate) . Cupt.readCupt
--       let readData = Task.readDataWith config
--       trainData <- readData trainPath
--       devData   <- case devPath of
--         Nothing -> return []
--         Just path -> readData path
--       paraMap <- Task.train (Cfg.baseFeatConfig config) sgdCfg trainData devData
--
--       putStrLn "# MWE identification model"
--       typSet <- Task.retrieveTypes . map Cupt.decorate <$> Cupt.readCupt trainPath
--       let readDataMWE
--             = fmap (map (Task.encodeTypeAmbiguity typSet))
--             . Task.readDataWith config
-- --       let readDataMWE
-- --             = fmap
-- --               ( map (Task.encodeTypeAmbiguity typSet)
-- --                 . mapMaybe (Task.encodeCupt . Cupt.decorate)
-- --               )
-- --             . Cupt.readCupt
--       trainDataMWE <- readDataMWE trainPath
--       devDataMWE <- case devPath of
--         Nothing -> return []
--         Just path -> readDataMWE path
--       paraMapMWE <- Task.train (Cfg.mweFeatConfig config) sgdCfg trainDataMWE devDataMWE
-- --       case outPathMWE of
-- --         Just path -> Map.save keyEnc paraMapMWE path
-- --         Nothing -> return ()
--
--       let model = Model.Model
--             { Model.paraMapBase = paraMap
--             , Model.paraMapMwe = paraMapMWE
--             , Model.mweTypSet = typSet
--             }
--       case modelDirMay of
--         Just path -> Model.save model path
--         Nothing -> return ()

    LiftCase -> do
      xs <- Cupt.parseCupt <$> TL.getContents
      let ys = map Task.liftCase xs
      TL.putStrLn (Cupt.renderCupt ys)

    RemoveDeriv -> do
      xs <- Cupt.parseCupt <$> TL.getContents
      let ys = map Task.removeDeriv xs
      TL.putStrLn (Cupt.renderCupt ys)

    CopyLemma -> do
      xs <- Cupt.parseCupt <$> TL.getContents
      let ys = map Task.copyLemma xs
      TL.putStrLn (Cupt.renderCupt ys)

    Clear -> do
      xs <- Cupt.parseCupt <$> TL.getContents
      let ys = map Task.removeMweAnnotations xs
      TL.putStrLn (Cupt.renderCupt ys)

    DepStats -> do
      xs <- Cupt.parseCupt <$> TL.getContents
      let statMap = Task.depRelStats xs
          n = sum (M.elems statMap)
          statList = sortBy (comparing snd) (M.toList statMap)
      forM_ (reverse statList) $ \(dep, k) -> do
        putStr $ T.unpack dep
        putStr ":\t"
        putStr $ show k
        putStr "\t("
        putStr $ show (fromIntegral k / fromIntegral n :: Double)
        putStrLn ")"


main :: IO ()
main =
    execParser optsExt >>= run
  where
    optsExt = info (helper <*> opts)
       ( fullDesc
      <> progDesc "MWE identification based on tree-structured CRFs"
      <> header "departage" )
