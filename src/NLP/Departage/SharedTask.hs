{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}


-- | Solution for the MWE identification shared task.


module NLP.Departage.SharedTask
  ( MWE
  , encodeCupt
  , train
  ) where


import           Control.Monad (forM)
import           Control.Monad.IO.Class (liftIO)

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


----------------------------------------------
-- MWEs
----------------------------------------------


-- | Information about MWE
type MWE = Bool


----------------------------------------------
-- Encoding as dependency tree
----------------------------------------------


-- !!!TODO!!!: make sure, that the order of tokens is preserved (as far as it is
-- possible)


-- | Encode a Cupt sentence as a dependency tree.
encodeCupt :: Cupt.Sent -> AH.DepTree MWE Cupt.Token
encodeCupt toks0 =

  case S.toList (childMap M.! [0]) of
    [rootID] -> go rootID
    _ -> error "SharedTask.encodeCupt: several roots?"

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


----------------------------------------------
-- Features, training
----------------------------------------------


-- | Type of the binary feature -- does it correspond to the parent, or the
-- sister node?
data BinType
  = Parent
  | Sister
  deriving (Show, Eq, Ord)


-- | CRF feature type. TODO: somehow add info about dependency labels.
data Feat
  = Binary1
    { prevTag :: MWE
    , currTag :: MWE
    , prevOrth :: T.Text
    , binType :: BinType
    }
  | Binary2
    { prevTag :: MWE
    , currTag :: MWE
    , currOrth :: T.Text
    , binType :: BinType
    }
  | Unary
    { currTag :: MWE
    , currOrth :: T.Text
    }
  | C
    { ok :: Bool
    }
--   | CU
--     { currTag :: MWE
--     , currGold :: MWE
--     }
--   | CB
--     { currTag :: MWE
--     , currGold :: MWE
--     , prevTag :: MWE
--     , prevGold :: MWE
--     }
  deriving (Show, Eq, Ord)


-- | Create a CRF training element.
mkElem
  :: AH.DepTree MWE Cupt.Token
  -> CRF.Elem IO (Map.RefMap IO) Feat F.LogFloat
mkElem depTree =

  CRF.Elem
  { CRF.elemHype = hype
  , CRF.elemProb = F.logFloat <$> AH.arcProb encoded
  , CRF.elemFeat = CRF.defaultFeat $ \arcID -> do
      let arcHead = H.head arcID hype
          arcTail = H.tail arcID hype
--           featList = case S.toList arcTail of
--             [] -> mkUnary arcHead
--             xs -> concatMap (mkBinary arcHead) xs
          featList = mkCheat arcHead $ S.toList arcTail 
      Map.newRefMap . M.fromList $ map (,1) featList
  }

  where

    encoded = AH.encodeAsHype depTree
    hype =  AH.encHype encoded

--     mkBinary arcHead arcTail =
--       let
--         (headMWE, headTok) = AH.nodeLabel encoded arcHead
--         (tailMWE, tailTok) = AH.nodeLabel encoded arcTail
--         isParent = Cupt.dephead headTok == Cupt.tokID tailTok
--         typ = if isParent then Parent else Sister
--       in
--         [ Binary1
--           { prevTag = tailMWE
--           , currTag = headMWE
--           , prevOrth = Cupt.orth tailTok
--           , binType = typ
--           }
--         , Binary2
--           { prevTag = tailMWE
--           , currTag = headMWE
--           , currOrth = Cupt.orth headTok
--           , binType = typ
--           }
--         ]
-- --         [ Unary
-- --           { currTag = headMWE
-- --           , currOrth = Cupt.orth headTok
-- --           }
-- --         ]
--
--     mkUnary arcHead =
--       let
--         (headMWE, headTok) = AH.nodeLabel encoded arcHead
--       in
--         Unary
--         { currTag = headMWE
--         , currOrth = Cupt.orth headTok
--         }

    mkCheat arcHead arcTails =
      let
        isOK (mwe, tok) = mwe /= (Cupt.mwe tok == [])
      in
        [ C . all isOK $ map (AH.nodeLabel encoded) (arcHead:arcTails)
        ]


-- | Train disambiguation module.
train
    :: SGD.SgdArgs                  -- ^ SGD configuration
    -> [AH.DepTree MWE Cupt.Token]  -- ^ Training data
    -- -> [AH.DepTree MWE Cupt.Token]  -- ^ Evaluation data
    -> IO ()
train sgdArgsT trainData = do -- evalData = do

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
  let fromSent = mkElem

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
                putStr "min param: " >> print (take 1 . L.sort $ M.elems m)
                putStr "max param: " >> print (take 1 . reverse . L.sort $ M.elems m)
            -- probability
            liftIO $ putStr "train probability: "
            -- batch <- withPhi =<< liftIO (map fromSent <$> trainData)
            batch <- withPhi (map fromSent trainData)
            liftIO . print . F.logFromLogFloat . product $ map (uncurry CRF.probability) batch

  -- gradient computation
  let computeGradient grad batch0 = do
        batch <- withPhi $ map fromSent batch0
        CRF.gradOn batch grad
        liftIO $ do
          putStrLn "# GRADIENT"
          gradMap <- Ref.readPrimRef $ Map.unRefMap grad
          print gradMap

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

  return ()
