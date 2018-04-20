{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


import           Data.Monoid ((<>))
import           Options.Applicative
import           Data.Maybe (mapMaybe)

import qualified NLP.Departage.CRF.SGD as SGD
import qualified NLP.Departage.CRF.Map as Map
import qualified NLP.Departage.DepTree.Cupt as Cupt
import qualified NLP.Departage.SharedTask as Task


--------------------------------------------------
-- Commands
--------------------------------------------------


data Command
    = Train
      { trainPath :: FilePath
        -- ^ Train dataset
      , devPath :: Maybe FilePath
        -- ^ Dev dataset (optional)
      , outPathBasic :: Maybe FilePath
        -- ^ The path to store the basic model (optional)
      , outPathMWE :: Maybe FilePath
        -- ^ The path to store the MWE model (optional)
      }
    | Tag
      { basicPath :: FilePath
        -- ^ Basic model params
      , mwePath :: FilePath
        -- ^ MWE model params
      , inpPath :: FilePath
        -- ^ Dataset to tag
      , outPath :: FilePath
        -- ^ Output file
      }


--------------------------------------------------
-- Parse options
--------------------------------------------------


trainOptions :: Parser Command
trainOptions = Train
  <$> strOption
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
        ( metavar "OUTPUT-BASIC"
       <> long "out-basic"
       <> short 'b'
       <> help "Output basic model"
        )
  <*> (optional . strOption)
        ( metavar "OUTPUT-MWE"
       <> long "out-mwe"
       <> short 'm'
       <> help "Output MWE model"
        )


tagOptions :: Parser Command
tagOptions = Tag
  <$> strOption
        ( metavar "BASIC"
       <> long "basic"
       <> short 'b'
       <> help "Basic model file"
        )
  <*> strOption
        ( metavar "MWE"
       <> long "mwe"
       <> short 'm'
       <> help "MWE model file"
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
  )


--------------------------------------------------
-- Main
--------------------------------------------------


-- | Run program depending on the cmdline arguments.
run :: Command -> IO ()
run cmd =

  case cmd of

    Train{..} -> do

      putStrLn "# Basic model"
      let readData = fmap (mapMaybe $ Task.encodeCupt . Cupt.decorate) . Cupt.readCupt
      trainData <- readData trainPath
      devData   <- case devPath of
        Nothing -> return []
        Just path -> readData path
      paraMap <- Task.train sgdCfg trainData devData
      case outPathBasic of
        Just path -> Map.save keyEnc paraMap path
        Nothing -> return ()

      putStrLn "# MWE identification model"
      typSet <- Task.retrieveTypes . map Cupt.decorate <$> Cupt.readCupt trainPath
      let readDataMWE
            = fmap
              ( map (Task.encodeTypeAmbiguity typSet)
                . mapMaybe (Task.encodeCupt . Cupt.decorate)
              )
            . Cupt.readCupt
      trainDataMWE <- readDataMWE trainPath
      devDataMWE <- case devPath of
        Nothing -> return []
        Just path -> readDataMWE path
      paraMapMWE <- Task.train sgdCfg trainDataMWE devDataMWE
      case outPathMWE of
        Just path -> Map.save keyEnc paraMapMWE path
        Nothing -> return ()

    Tag{..} -> do
      paraMap <- Map.load keyEnc basicPath
      paraMapMWE <- Map.load keyEnc mwePath
      typSet <- Task.retrieveTypes' paraMapMWE
      putStr "# MWE types: " >> print typSet
      Task.tagFile typSet paraMap paraMapMWE inpPath outPath

  where

    keyEnc = Map.Encoding
      { Map.toStr = show
      , Map.fromStr = read
      }

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
