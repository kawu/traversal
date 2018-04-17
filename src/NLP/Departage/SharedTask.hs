{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Solution for the MWE identification shared task.


module NLP.Departage.SharedTask
  ( MWE
  , encodeCupt
  , decodeCupt
  , train
  , tagFile
  ) where


import           Control.Monad (forM)
import           Control.Monad.IO.Class (liftIO)

import           Data.Ord (comparing)
import           Data.Maybe (maybeToList, mapMaybe)
import qualified Data.List as L
import qualified Data.Tree as R
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Data.Number.LogFloat as F
import qualified Data.PrimRef as Ref

import qualified NLP.Departage.Hype as H
import qualified NLP.Departage.DepTree.Cupt as Cupt
import qualified NLP.Departage.DepTree.AsHype as AH
import qualified NLP.Departage.Prob as P

import qualified NLP.Departage.CRF as CRF
import qualified NLP.Departage.CRF.SGD as SGD
import qualified NLP.Departage.CRF.SGD.Dataset as SGD.Dataset
import qualified NLP.Departage.CRF.Map as Map
import qualified NLP.Departage.CRF.Mame as Mame

import Debug.Trace (trace)


----------------------------------------------
-- MWEs
----------------------------------------------


-- | Information about MWE
type MWE = Bool


----------------------------------------------
-- Encoding as dependency tree
----------------------------------------------


-- !!!TODO!!!: make sure, that the surface ordering of tokens is preserved!!!


-- | Encode a Cupt sentence as a dependency tree.
encodeCupt :: Cupt.Sent -> Maybe (AH.DepTree MWE Cupt.Token)
encodeCupt toks0 =

  case S.toList (childMap M.! Cupt.TokID 0) of
    [rootID] -> Just $ go rootID
    _ -> trace "SharedTask.encodeCupt: several roots?" Nothing

  where

    go tokID = R.Node
      { R.rootLabel = (tokLabel, tok)
      , R.subForest = subForest
      }
      where
        tok = tokMap M.! tokID
        tokLabel =
          if Cupt.mwe tok == []
          then P.fromList [(True, 0.0), (False, 1.0)]
          else P.fromList [(True, 1.0), (False, 0.0)]
        subForest =
          case M.lookup tokID childMap of
            Nothing -> []
            Just children -> map go (S.toList children)

    childMap =
      M.fromListWith S.union
      [ (Cupt.dephead tok, S.singleton $ Cupt.tokID tok)
      | tok <- toks ]
    tokMap =
      M.fromList
      [ (Cupt.tokID tok, tok)
      | tok <- toks ]

    -- only segmentation-chosen tokens
    toks = filter Cupt.chosen toks0


-- | Decode a Cupt sentence from a dependency tree.
decodeCupt :: AH.DepTree MWE Cupt.Token -> Cupt.Sent
decodeCupt =
  L.sortBy (comparing position) . R.flatten . fmap decodeTok
  where
    position tok = case Cupt.tokID tok of
      Cupt.TokID x -> (x, -1)
      Cupt.TokIDRange x y -> (x, x - y)
    decodeTok (prob, tok) =
      case M.lookup True (P.unProb prob) of
        Just p  ->
          if p > 0.5
          then tok {Cupt.mwe = [(1, Just . T.pack $ "MWE:" ++ show p)]}
          else tok {Cupt.mwe = []}
        Nothing -> tok {Cupt.mwe = []}
--       case M.lookupMax (P.unProb prob) of
--         Just (True, _) -> tok {Cupt.mwe = [(0, Just "MWE")]}
--         _ -> tok {Cupt.mwe = []}


----------------------------------------------
-- Features, training
----------------------------------------------


-- -- | Type of the binary feature -- does it correspond to the parent, or the
-- -- sister node?
-- data BinType
--   = Parent
--   | Sister
--   deriving (Show, Eq, Ord)


-- | CRF feature type. TODO: somehow add info about dependency labels.
data Feat
  = ParentFeat
    { parentTag :: MWE
    , currTag :: MWE
    , parentOrth :: T.Text
    , currOrth :: T.Text
    }
  | SisterFeat
    { prevTag :: MWE
    , currTag :: MWE
    , prevOrth :: T.Text
    , currOrth :: T.Text
    }
--   | Binary1
--     { prevTag :: AH.Label MWE
--     , currTag :: AH.Label MWE
--     , prevOrth :: T.Text
--     , binType :: BinType
--     }
--   | Binary2
--     { prevTag :: AH.Label MWE
--     , currTag :: AH.Label MWE
--     , currOrth :: T.Text
--     , binType :: BinType
--     }
  | Unary
    { currTag :: MWE
    , currOrth :: T.Text
    }
--   | C
--     { ok :: Bool
--     }
  deriving (Show, Eq, Ord)


-- | Create a CRF training element.
mkElem
  -- :: AH.DepTree MWE Cupt.Token
  :: AH.EncHype MWE Cupt.Token
  -> CRF.Elem IO (Map.RefMap IO) Feat F.LogFloat
-- mkElem depTree =
mkElem encoded =

  CRF.Elem
  { CRF.elemHype = hype
  , CRF.elemProb = F.logFloat <$> AH.encArcProb encoded
  , CRF.elemFeat = CRF.defaultFeat $ \arcID -> do
      let arcHead = H.head arcID hype
          arcTail = H.tail arcID hype
          featList =
            mkUnary arcHead ++
            concatMap (mkBinary arcHead) (S.toList arcTail)
      Map.newRefMap . M.fromList $ map (,1) featList
  }

  where

    -- encoded = AH.encodeTree depTree
    hype =  AH.encHype encoded

    mkBinary arcHead arcTail
      | isSister =
          [ SisterFeat
            { currTag = AH.origLabel tailMWE
            , prevTag = AH.origLabel headMWE
            , currOrth = Cupt.orth tailTok
            , prevOrth = Cupt.orth headTok
            }
          ]
      | otherwise = []
      where
        headMWE = AH.encLabel encoded arcHead
        headTok = AH.encToken encoded (AH.encNode encoded arcHead)
        tailMWE = AH.encLabel encoded arcTail
        tailTok = AH.encToken encoded (AH.encNode encoded arcTail)
        -- (headMWE, headTok) = AH.nodeLabel encoded arcHead
        -- (tailMWE, tailTok) = AH.nodeLabel encoded arcTail
        isSister = Cupt.dephead tailTok /= Cupt.tokID headTok

    mkUnary arcHead =
      let
        -- (headMWE, headTok) = AH.nodeLabel encoded arcHead
        headMWE = AH.encLabel encoded arcHead
        headTok = AH.encToken encoded (AH.encNode encoded arcHead)
        unary = Unary
          { currTag = AH.origLabel headMWE
          , currOrth = Cupt.orth headTok
          }
        binary = maybeToList $ do
          parTok <- AH.encParent encoded arcHead
          parTag <- AH.parentLabel headMWE
          return $ ParentFeat
            { parentTag = parTag
            , currTag = AH.origLabel headMWE
            , parentOrth = Cupt.orth parTok
            , currOrth = Cupt.orth headTok
            }
      in
        unary : binary

--     mkCheat arcHead arcTails =
--       let
--         isOK (mwe, tok) = AH.origLabel mwe /= (Cupt.mwe tok == [])
--       in
--         [ C . all isOK $ map (AH.nodeLabel encoded) (arcHead:arcTails)
--         ]


-- | A particular CRF model (IO embedded parameter map).
type ParaMap = Map.RefMap IO Feat Double


-- | Train disambiguation module.
train
    :: SGD.SgdArgs                  -- ^ SGD configuration
    -> [AH.DepTree MWE Cupt.Token]  -- ^ Training data
    -> [AH.DepTree MWE Cupt.Token]  -- ^ Development data
    -> IO ParaMap
train sgdArgsT trainData devData = do

  -- parameter map
  paraMap <- Map.newRefMap M.empty

  -- parameters
  let
    trainSize = length trainData
    batchSize = SGD.batchSize sgdArgsT

    doneTotal :: Int -> Int
    doneTotal = floor . done

    done :: Int -> Double
    done i
        = fromIntegral (i * batchSize)
        / fromIntegral trainSize

  -- transforming data to the CRF form
  let fromSent = mkElem . AH.encodeTree

  -- enrich the dataset elements with potential functions, based on the current
  -- parameter values (and using `defaultPotential`)
  let withPhi xs = do
        paraPure <- liftIO $ Map.unsafeFreeze paraMap
        let crf x = F.logToLogFloat $ case paraPure x of
              Nothing -> 0
              Just v  -> v
        forM xs $ \x@CRF.Elem{..} -> do
          phi <- CRF.defaultPotential crf elemHype elemFeat
          return (x, phi)

  -- dataset probability
  let dataProb dataset = do
        -- batch <- withPhi =<< liftIO (map fromSent <$> trainData)
        batch <- withPhi (map fromSent dataset)
        return . F.logFromLogFloat . product $ map (uncurry CRF.probability) batch

  -- notification function
  let notify k
        | doneTotal k == doneTotal (k - 1) =
            liftIO $ putStr "."
        | otherwise = do
            -- parameters
            liftIO $ do
              putStrLn ""
              Ref.readPrimRef (Map.unRefMap paraMap) >>= \m -> do
                putStr "num param: " >> print (M.size m)
                let sortParams = L.sort . M.elems
                putStr "min param: " >> print (take 1 $ sortParams m)
                putStr "max param: " >> print (take 1 . reverse $ sortParams m)
                -- let sortParams = L.sortBy (comparing snd) . M.toList
                -- putStrLn "min params: " >> mapM_ print (take 10 $ sortParams m)
                -- putStrLn "max params: " >> mapM_ print (take 10 . reverse $ sortParams m)
                -- putStrLn "params: " >> mapM_ print (sortParams m)
            -- probability
            liftIO $ putStr "train probability: "
            liftIO . print =<< dataProb trainData
            liftIO $ putStr "dev probability: "
            liftIO . print =<< dataProb devData

  -- gradient computation
  let computeGradient grad batch0 = do
        batch <- withPhi $ map fromSent batch0
        CRF.gradOn batch grad
--         liftIO $ do
--           putStrLn "# GRADIENT"
--           gradMap <- Ref.readPrimRef $ Map.unRefMap grad
--           print gradMap

--   -- SGD parameters
--   let sgdParams = SGD.sgdArgsDefault
--         { SGD.batchSize = batchSize
--         , SGD.iterNum = 10
--         , SGD.regVar = 4
--         , SGD.gain0 = 0.25
--         , SGD.tau = 5
--         , SGD.gamma = 0.9
--         }

  -- Buffer creation
  let makeBuff = Mame.Make
        { Mame.mkValueBuffer = Map.newRefMap M.empty
        , Mame.mkDoubleBuffer = Map.newRefMap M.empty
        }

  -- pureTrainData <- trainData
  let pureTrainData = trainData
  SGD.Dataset.withVect pureTrainData $ \dataSet -> do
    -- TODO: change the type of `sgd` so that it runs internally `runMame` with
    -- provided `makeBuff`
    Mame.runMame makeBuff $ do
      SGD.sgd
        sgdArgsT
        notify
        computeGradient
        dataSet
        paraMap

  return paraMap


-- | Tag the dependency tree given the model parameters.
--
-- NOTE: the input tree may have some MWE annotations. These will be ignored and
-- replaced by model-based annotations.
tagOne
  :: ParaMap
  -> AH.DepTree MWE Cupt.Token
  -> Mame.Mame (Map.RefMap IO) Feat F.LogFloat IO (AH.DepTree MWE Cupt.Token)
tagOne paraMap depTree = do
  let encTree = AH.encodeTree depTree
      crfElem = mkElem encTree
  -- START: computing arc potentials (UNSAFELY?)
  paraPure <- liftIO $ Map.unsafeFreeze paraMap
  let crf x = F.logToLogFloat $ case paraPure x of
        Nothing -> 0
        Just v  -> v
  phi <- CRF.defaultPotential crf (CRF.elemHype crfElem) (CRF.elemFeat crfElem)
  -- END: computing arc potentials
  let (prob, _) = CRF.marginals phi (AH.encHype encTree)
      depTree' = AH.decodeTree encTree prob
  return depTree'


-- | Tag several dependency trees (using `tagOne`).
tagMany
  :: ParaMap
  -> [AH.DepTree MWE Cupt.Token]
  -> IO [AH.DepTree MWE Cupt.Token]
tagMany paraMap depTrees = do
  let makeBuff = Mame.Make
        { Mame.mkValueBuffer = Map.newRefMap M.empty
        , Mame.mkDoubleBuffer = Map.newRefMap M.empty
        }
  Mame.runMame makeBuff $ do
    mapM (tagOne paraMap) depTrees


-- | High-level tagging function.
tagFile
  :: ParaMap
  -> FilePath -- ^ Input file
  -> FilePath -- ^ Output file
  -> IO ()
tagFile paraMap inpFile outFile = do
  xs <- mapMaybe encodeCupt <$> Cupt.readCupt inpFile
  ys <- tagMany paraMap xs
  Cupt.writeCupt (map decodeCupt ys) outFile
