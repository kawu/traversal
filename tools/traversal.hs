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
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL

import           System.FilePath (isAbsolute, (</>))

import qualified Dhall as Dhall

import qualified NLP.Traversal.CRF.SGD as SGD
import qualified NLP.Traversal.DepTree.Cupt as Cupt
import qualified NLP.Traversal.SharedTask as Task
import qualified NLP.Traversal.Config as Cfg
import qualified NLP.Traversal.Model as Model


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
      , bestPath :: Bool
        -- ^ Use best path instead of marginals
      , mweType :: Maybe T.Text
        -- ^ Load a simple model and focus on a single MWE type
      }
    | LiftCase
    | PrepareSL
    | RemoveDeriv
    | CopyLemma
    | Clear
    | DepStats
    | MweStats
    | Merge
      { origFile :: FilePath
        -- ^ Original file
      , systemFile :: FilePath
        -- ^ File with missing tokens
      }
    | Conn
      { seqModel :: Bool
        -- ^ Use sequential model instead of dependency tree
      , parOnly :: Bool
        -- ^ Consider nodes adjacent only in case of the parent/child relation
        -- (and not in case of the sibling relation)
      , checkSep :: Bool
        -- ^ Not only check that MWEs are connected, but also separated
      }


--------------------------------------------------
-- Parse options
--------------------------------------------------


mergeOptions :: Parser Command
mergeOptions = Merge
  <$> strOption
        ( metavar "FILE"
       <> long "orig"
       <> short 'o'
       <> help "Original file"
        )
  <*> strOption
        ( metavar "FILE"
       <> long "system"
       <> short 's'
       <> help "System (tagged) file"
        )


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


prepareSLOptions :: Parser Command
prepareSLOptions = pure PrepareSL


removeDerivOptions :: Parser Command
removeDerivOptions = pure RemoveDeriv


copyLemmaOptions :: Parser Command
copyLemmaOptions = pure CopyLemma


clearOptions :: Parser Command
clearOptions = pure Clear


connOptions :: Parser Command
connOptions = Conn
  <$> switch
        ( long "seq"
       <> help "Use sequential model instead of dependency tree"
        )
  <*> switch
        ( long "par"
       <> short 'p'
       <> help "Consider nodes adjacent only in case of the parent/child relation"
        )
  <*> switch
        ( long "sep"
       <> help "Not only check that MWEs are connected, but also separated"
        )


depStatsOptions :: Parser Command
depStatsOptions = pure DepStats


mweStatsOptions :: Parser Command
mweStatsOptions = pure MweStats


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
    <> command "merge"
    (info (helper <*> mergeOptions)
      (progDesc "Merge two files")
    )
    <> command "liftcase"
    (info (helper <*> liftCaseOptions)
      (progDesc "Lift case markers (stdin -> stdout)")
    )
    <> command "preparesl"
    (info (helper <*> prepareSLOptions)
      (progDesc "Prepare SL (stdin -> stdout)")
    )
    <> command "removederiv"
    (info (helper <*> removeDerivOptions)
      (progDesc "Remove DERIVs (stdin -> stdout)")
    )
    <> command "copylemma"
    (info (helper <*> copyLemmaOptions)
      (progDesc "Copy lemma from orth where not present (TR; stdin -> stdout)")
    )
    <> command "clear"
    (info (helper <*> clearOptions)
      (progDesc "Clear MWE annotations")
    )
    <> command "depstats"
    (info (helper <*> depStatsOptions)
      (progDesc "Show dependency relation statistics")
    )
    <> command "mwestats"
    (info (helper <*> mweStatsOptions)
      (progDesc "Show MWE statistics")
    )
    <> command "conn"
    (info (helper <*> connOptions)
      (progDesc "Compute the percentage of connected MWEs")
    )
  )


--------------------------------------------------
-- Main
--------------------------------------------------


-- | Run program depending on the cmdline arguments.
run :: Command -> IO ()
run cmd =

  case cmd of

--     Tag{..} -> do
--       let configPath' =
--             if isAbsolute configPath
--             then configPath
--             else "./" </> configPath
--           tagConfig = Task.TagConfig
--             { tagBestPath = bestPath
--             , splitMwesOn = splitOn }
--       config <- Dhall.detailed (Dhall.input Dhall.auto $ fromString configPath')
--       case mweType of
--         Nothing -> do
--           model <- Model.loadEnsemble modelPath
--           putStr "# MWE types: " >> print (Model.mweTypSet model)
--           Task.tagEnsemble config tagConfig model -- inpPath outPath
--         Just mweTyp -> do
--           model <- Model.loadSimple modelPath
--           Task.tagSimple config tagConfig mweTyp model -- inpPath outPath

    Tag{..} -> do
      let configPath' =
            if isAbsolute configPath
            then configPath
            else "./" </> configPath
          tagConfig = Task.TagConfig
            { tagBestPath = bestPath
            , splitMwesOn = Nothing }
      config <- Dhall.detailed (Dhall.input Dhall.auto $ fromString configPath')
      case mweType of
        Nothing -> error "not supported anymore!"
        Just mweTyp -> do
          model <- Model.loadIOB modelPath
          Task.tagIOB config tagConfig mweTyp model

    Train{..} -> do
      let configPath' =
            if isAbsolute configPath
            then configPath
            else "./" </> configPath
      config <- Dhall.detailed (Dhall.input Dhall.auto $ fromString configPath')
      case mweType of
        Nothing -> error "not supported anymore!"
        Just mweTyp -> do
          model <- Task.trainSimpleIOB config mweTyp trainPath devPath
          case modelPathMay of
            Just path -> Model.saveIOB model path
            Nothing -> return ()

--     Train{..} -> do
--       let configPath' =
--             if isAbsolute configPath
--             then configPath
--             else "./" </> configPath
--       config <- Dhall.detailed (Dhall.input Dhall.auto $ fromString configPath')
--       case mweType of
--         Nothing -> do
--           model <- Task.trainEnsemble config trainPath devPath
--           case modelPathMay of
--             Just path -> Model.saveEnsemble model path
--             Nothing -> return ()
--         Just mweTyp -> do
--           model <- Task.trainSimple config mweTyp trainPath devPath
--           case modelPathMay of
--             Just path -> Model.saveSimple model path
--             Nothing -> return ()

    Merge{..} -> do
      xss <- Cupt.readCupt origFile
      yss <- Cupt.readCupt systemFile
      let zss = Cupt.mergeCupt xss yss
      TL.putStrLn $ Cupt.renderCupt zss

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
      let ys = map2 Task.liftCase xs
      TL.putStrLn (Cupt.renderCupt ys)

    PrepareSL -> do
      xs <- Cupt.parseCupt <$> TL.getContents
      let ys = map2 (Task.liftDolD . Task.copyUpos1) xs
      TL.putStrLn (Cupt.renderCupt ys)

    RemoveDeriv -> do
      xs <- Cupt.parseCupt <$> TL.getContents
      let ys = map2 Task.removeDeriv xs
      TL.putStrLn (Cupt.renderCupt ys)

    CopyLemma -> do
      xs <- Cupt.parseCupt <$> TL.getContents
      let ys = map2 Task.copyLemma xs
      TL.putStrLn (Cupt.renderCupt ys)

--     CopyUpos -> do
--       xs <- Cupt.parseCupt <$> TL.getContents
--       let ys = map Task.copyUpos xs
--       TL.putStrLn (Cupt.renderCupt ys)

    Clear -> do
      xs <- Cupt.parseCupt <$> TL.getContents
      let ys = map2 Task.removeMweAnnotations xs
      TL.putStrLn (Cupt.renderCupt ys)

    Conn{..} -> do
      xs <- map Cupt.decorate . concat . Cupt.parseCupt
        <$> TL.getContents
      let cfg = Task.defaultConnCfg
            { Task.connSeq = seqModel
            , Task.connParOnly = parOnly
            , Task.connCheckSep = checkSep
            }
          statMap = Task.connectedMWEs' cfg xs
          printPair (x, y) = do
            putStr "("
            putStr $ show x
            putStr "/"
            putStr $ show y
            putStrLn ")"
      forM_ (M.toList statMap) $ \(mweCat, (k, n)) -> do
        T.putStr mweCat
        putStr " => "
        printPair (k, n)
      let k = sum . map fst $ M.elems statMap
          n = sum . map snd $ M.elems statMap
      putStr "TOTAL => "
      putStr $ show (roundTo 2 . (*100) $ fromIntegral k / fromIntegral n)
      putStr " "
      printPair (k, n)

    DepStats -> do
      xs <- map Cupt.decorate . concat . Cupt.parseCupt
        <$> TL.getContents
      let statMap = Task.depRelStats xs
          n = sum (M.elems statMap)
          statList = sortBy (comparing snd) (M.toList statMap)
      forM_ (reverse statList) $ \(dep, k) -> do
        putStr $ show dep
        putStr ":\t"
        putStr $ show k
        putStr "\t("
        putStr $ show (fromIntegral k / fromIntegral n :: Double)
        putStrLn ")"

    MweStats -> do
      xs <- map Cupt.decorate . concat . Cupt.parseCupt
        <$> TL.getContents
      let statMap = Task.mweStats xs
          n = sum (M.elems statMap)
          statList = sortBy (comparing snd) (M.toList statMap)
      forM_ (reverse statList) $ \(mwe, k) -> do
        putStr $ T.unpack mwe
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


----------------------------------------------
-- Pure utils
----------------------------------------------


map2 :: (a -> b) -> [[a]] -> [[b]]
map2 = map . map


roundTo :: Int -> Double -> Double
roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
