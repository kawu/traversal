{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


import           Data.Monoid ((<>))
import           Options.Applicative
-- import           Data.Maybe (mapMaybe)
import           Data.String (fromString)
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
      , modelDirMay :: Maybe FilePath
        -- ^ Directory path to store the model (optional)
      }
    | Tag
      { configPath :: FilePath
        -- ^ Feature configuration path
      , modelDir :: FilePath
        -- ^ Model directory
      , inpPath :: FilePath
        -- ^ Dataset to tag
      , outPath :: FilePath
        -- ^ Output file
      }
    | LiftCase


--------------------------------------------------
-- Parse options
--------------------------------------------------


trainOptions :: Parser Command
trainOptions = Train
  <$> strOption
        ( metavar "FEAT-CONFIG"
       <> long "feat"
       <> short 'f'
       <> help "Feature configuration file"
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
       <> long "out"
       <> short 'o'
       <> help "Output model directory"
        )


tagOptions :: Parser Command
tagOptions = Tag
  <$> strOption
        ( metavar "FEAT-CONFIG"
       <> long "feat"
       <> short 'f'
       <> help "Feature configuration file"
        )
  <*> strOption
        ( metavar "MODEL-DIR"
       <> long "model"
       <> short 'm'
       <> help "Model directory"
        )
  <*> strOption
        ( metavar "INPUT"
       <> long "input"
       <> short 'i'
       <> help "Input file"
        )
  <*> strOption
        ( metavar "OUTPUT"
       <> long "output"
       <> short 'o'
       <> help "Output file"
        )


liftCaseOptions :: Parser Command
liftCaseOptions = pure LiftCase


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
  )


--------------------------------------------------
-- Main
--------------------------------------------------


-- | Run program depending on the cmdline arguments.
run :: Command -> IO ()
run cmd =

  case cmd of

    Train{..} -> do
      -- featCfg <- Cfg.loadConfig configPath
      let configPath' =
            if isAbsolute configPath
            then configPath
            else "./" </> configPath
      config <- Dhall.detailed (Dhall.input Dhall.auto $ fromString configPath')

      putStrLn "# Basic model"
      -- let readData = fmap (mapMaybe $ Task.encodeCupt . Cupt.decorate) . Cupt.readCupt
      let readData = Task.readDataWith config
      trainData <- readData trainPath
      devData   <- case devPath of
        Nothing -> return []
        Just path -> readData path
      paraMap <- Task.train (Cfg.baseFeatConfig config) sgdCfg trainData devData

      putStrLn "# MWE identification model"
      typSet <- Task.retrieveTypes . map Cupt.decorate <$> Cupt.readCupt trainPath
      let readDataMWE
            = fmap (map (Task.encodeTypeAmbiguity typSet))
            . Task.readDataWith config
--       let readDataMWE
--             = fmap
--               ( map (Task.encodeTypeAmbiguity typSet)
--                 . mapMaybe (Task.encodeCupt . Cupt.decorate)
--               )
--             . Cupt.readCupt
      trainDataMWE <- readDataMWE trainPath
      devDataMWE <- case devPath of
        Nothing -> return []
        Just path -> readDataMWE path
      paraMapMWE <- Task.train (Cfg.mweFeatConfig config) sgdCfg trainDataMWE devDataMWE
--       case outPathMWE of
--         Just path -> Map.save keyEnc paraMapMWE path
--         Nothing -> return ()

      let model = Model.Model
            { Model.paraMapBase = paraMap
            , Model.paraMapMwe = paraMapMWE
            , Model.mweTypSet = typSet
            }
      case modelDirMay of
        Just path -> Model.save model path
        Nothing -> return ()

    Tag{..} -> do
      -- featCfg <- Cfg.loadConfig configPath
      let configPath' =
            if isAbsolute configPath
            then configPath
            else "./" </> configPath
      config <- Dhall.detailed (Dhall.input Dhall.auto $ fromString configPath')
      -- paraMap <- Map.load keyEnc basicPath
      -- paraMapMWE <- Map.load keyEnc mwePath
      model <- Model.load modelDir
      -- typSet <- Task.retrieveTypes' paraMapMWE
      putStr "# MWE types: " >> print (Model.mweTypSet model)
      Task.tagFile
        (Model.mweTypSet model)
        config
        -- paraMap
        (Model.paraMapBase model)
        -- paraMapMWE
        (Model.paraMapMwe model)
        inpPath outPath

    LiftCase -> do
      xs <- Cupt.parseCupt <$> TL.getContents
      let ys = map Task.liftCase xs
      TL.putStrLn (Cupt.renderCupt ys)

  where

    sgdCfg = SGD.sgdArgsDefault
      { SGD.iterNum=20
      , SGD.regVar=10.0
      , SGD.gamma=0.9
      , SGD.tau=5
      , SGD.gain0=0.1
      , SGD.batchSize=30
      }

main :: IO ()
main =
    execParser optsExt >>= run
  where
    optsExt = info (helper <*> opts)
       ( fullDesc
      <> progDesc "MWE identification based on tree-structured CRFs"
      <> header "departage" )
