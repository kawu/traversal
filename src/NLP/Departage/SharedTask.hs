{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Solution for the MWE identification shared task.


module NLP.Departage.SharedTask
  ( MWE
  , ParaMap
  , encodeCupt
  , decodeCupt
  , encodeTypeAmbiguity
  , retrieveTypes
  -- , retrieveTypes'
  , train
  , tagFile
  ) where


import           Control.Monad (forM, guard)
import           Control.Monad.IO.Class (liftIO)
-- import qualified Control.Monad.State.Strict as State
-- import qualified Pipes as Pipes

import           System.IO (hSetBuffering, stdout, BufferMode (..))

import           Data.Ord (comparing)
import           Data.Maybe (listToMaybe, maybeToList, mapMaybe)
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

import qualified NLP.Departage.FeatConfig as Cfg

import Debug.Trace (trace)


----------------------------------------------
-- Basic Types
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

  -- case S.toList (childMap M.! Cupt.TokID 0) of
  case S.toList (childMap M.! Cupt.rootParID) of
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
          then tok {Cupt.mwe = [(1, T.pack $ "MWE:" ++ show p)]}
          else tok {Cupt.mwe = []}
        Nothing -> tok {Cupt.mwe = []}
--       case M.lookupMax (P.unProb prob) of
--         Just (True, _) -> tok {Cupt.mwe = [(0, Just "MWE")]}
--         _ -> tok {Cupt.mwe = []}


-- | Decode a Cupt sentence from a dependency tree.
decodeCupt' :: AH.DepTree (Maybe Cupt.MweTyp) Cupt.Token -> Cupt.Sent
decodeCupt' =
  L.sortBy (comparing position) . R.flatten . fmap decodeTok
  where
    position tok = case Cupt.tokID tok of
      Cupt.TokID x -> (x, -1)
      Cupt.TokIDRange x y -> (x, x - y)
    decodeTok (prob, tok) =
      case listToMaybe . reverse . L.sortBy (comparing snd) $ M.toList (P.unProb prob) of
        Just (Just typ, _) -> tok {Cupt.mwe = [(0, typ)]}
        _ -> tok {Cupt.mwe = []}


----------------------------------------------
-- Encoding as dependency tree:
-- MWE-type-preserving version
----------------------------------------------


-- -- | What's the purpose of encoding type ambiguity?
-- data Purpose
--   = Parsing  -- ^ For parsing
--   | Training -- ^ For training
--   deriving (Show, Eq, Ord)


-- | Encode MWE-type ambiguity for nodes marked as (untyped) MWEs (more
-- precisely, for those nodes for which the the probability of being an MWE is >
-- 0.5).
encodeTypeAmbiguity
--   :: Purpose
--      -- ^ Inventory of permissible MWE types
  :: S.Set Cupt.MweTyp
     -- ^ Inventory of permissible MWE types
  -> AH.DepTree MWE Cupt.Token
     -- ^ Dependency tree marked with (untyped) MWEs
  -> AH.DepTree (Maybe Cupt.MweTyp) Cupt.Token
     -- ^ Dependency tree ambiguous w.r.t. MWE types
encodeTypeAmbiguity typSet =
  fmap encode
  where
    encode (prob, tok) = addTok tok $ do
      p <- M.lookup True (P.unProb prob)
      guard $ p > 0.5
      let typNum = fromIntegral . length $ Cupt.mwe tok
      return $ P.fromList $
        [(Just typ, 0.0) | typ <- S.toList typSet] ++
        [(Just typ, 1.0 / typNum) | (_mweID, typ) <- Cupt.mwe tok]
    addTok tok Nothing     = (P.fromList [(Nothing, 1.0)], tok)
    addTok tok (Just prob) = (prob, tok)


-- | Retrieve the set of types present in the given sentence.
retrieveTypes :: [Cupt.Sent] -> S.Set Cupt.MweTyp
retrieveTypes =
  let retrieve = map snd . Cupt.mwe
  in  S.fromList . concatMap retrieve . concat


-- -- | A version of `retrieveTypes` which takes a model as input.
-- retrieveTypes' :: ParaMap (Maybe Cupt.MweTyp) -> IO (S.Set Cupt.MweTyp)
-- retrieveTypes' paraMapRef = do
--   paraMap <- Ref.readPrimRef (Map.unRefMap paraMapRef)
--   return . S.fromList $ do
--     (para, _val) <- M.toList paraMap
--     Just tag <- case para of
--         ParentOrth{..} -> [parentTag, currTag]
--         ParentLemma{..} -> [parentTag, currTag]
--         SisterOrth{..} -> [prevTag, currTag]
--         SisterLemma{..} -> [prevTag, currTag]
--         UnaryOrth{..} -> [currTag]
--         UnaryLemma{..} -> [currTag]
--     return tag


----------------------------------------------
-- Features
----------------------------------------------


-- | CRF feature type. TODO: somehow add info about dependency labels.
data Feat mwe
  = ParentOrth
    { parentTag :: mwe
    , currTag :: mwe
    , parentOrth :: T.Text
    , currOrth :: T.Text
    }
  | ParentLemma
    { parentTag :: mwe
    , currTag :: mwe
    , parentLemma :: T.Text
    , currLemma :: T.Text
    }
  | ParentLemmaParent
    { parentTag :: mwe
    , currTag :: mwe
    , parentLemma :: T.Text
    }
  | ParentLemmaCurrent
    { parentTag :: mwe
    , currTag :: mwe
    , currLemma :: T.Text
    }
  | ParentTagsOnly
    { parentTag :: mwe
    , currTag :: mwe
    }
  | ParentTagsAndDepRel
    { parentTag :: mwe
    , currTag :: mwe
    , currRel :: T.Text
      -- ^ Dependency type of the arc between the node and its parent
    }
  | SisterOrth
    { prevTag :: mwe
    , currTag :: mwe
    , prevOrth :: T.Text
    , currOrth :: T.Text
    }
  | SisterLemma
    { prevTag :: mwe
    , currTag :: mwe
    , prevLemma :: T.Text
    , currLemma :: T.Text
    }
  | SisterLemmaSister
    { prevTag :: mwe
    , currTag :: mwe
    , prevLemma :: T.Text
    }
  | SisterLemmaCurrent
    { prevTag :: mwe
    , currTag :: mwe
    , currLemma :: T.Text
    }
  | SisterTagsOnly
    { prevTag :: mwe
    , currTag :: mwe
    }
  | SisterTagsAndDepRel
    { prevTag :: mwe
    , currTag :: mwe
    , prevRel :: T.Text
      -- ^ Dependency type of the arc between the sister node and its parent
    , currRel :: T.Text
      -- ^ Dependency type of the arc between the current node and its parent
    }
  | UnaryOrth
    { currTag :: mwe
    , currOrth :: T.Text
    }
  | UnaryLemma
    { currTag :: mwe
    , currLemma :: T.Text
    }
  deriving (Read, Show, Eq, Ord)


-- | Collect unary features for a given token/MWE pair.
collectUnary
  :: S.Set Cfg.UnaryOption
  -> (Cupt.Token, mwe)
  -> [Feat mwe]
collectUnary options (tok, mwe) =
  map collect (S.toList options)
  where
    collect option = case option of
      Cfg.UnaryOrth -> UnaryOrth
        { currTag = mwe
        , currOrth = Cupt.orth tok
        }
      Cfg.UnaryLemma -> UnaryLemma
        { currTag = mwe
        , currLemma = Cupt.lemma tok
        }


-- | Collect binary parent features.
collectParent
  :: S.Set Cfg.ParentOption
  -> (Cupt.Token, mwe) -- ^ Parent
  -> (Cupt.Token, mwe) -- ^ Current
  -> [Feat mwe]
collectParent options (parTok, parMwe) (curTok, curMwe) =
  map collect (S.toList options)
  where
    collect option = case option of
      Cfg.ParentOrth -> ParentOrth
        { parentTag = parMwe
        , currTag = curMwe
        , parentOrth = Cupt.orth parTok
        , currOrth = Cupt.orth curTok
        }
      Cfg.ParentLemma -> ParentLemma
        { parentTag = parMwe
        , currTag = curMwe
        , parentLemma = Cupt.lemma parTok
        , currLemma = Cupt.lemma curTok
        }
      Cfg.ParentLemmaParent -> ParentLemmaParent
        { parentTag = parMwe
        , currTag = curMwe
        , parentLemma = Cupt.lemma parTok
        }
      Cfg.ParentLemmaCurrent -> ParentLemmaCurrent
        { parentTag = parMwe
        , currTag = curMwe
        , currLemma = Cupt.lemma curTok
        }
      Cfg.ParentTagsOnly -> ParentTagsOnly
        { parentTag = parMwe
        , currTag = curMwe
        }
      Cfg.ParentTagsAndDepRel -> ParentTagsAndDepRel
        { parentTag = parMwe
        , currTag = curMwe
        , currRel = Cupt.deprel curTok
        }


-- | Collect binary sister features.
collectSister
  :: S.Set Cfg.SisterOption
  -> (Cupt.Token, mwe) -- ^ Sister
  -> (Cupt.Token, mwe) -- ^ Current
  -> [Feat mwe]
collectSister options (sisTok, sisMwe) (curTok, curMwe) =
  map collect (S.toList options)
  where
    collect option = case option of
      Cfg.SisterOrth -> SisterOrth
        { prevTag = sisMwe
        , currTag = curMwe
        , prevOrth = Cupt.orth sisTok
        , currOrth = Cupt.orth curTok
        }
      Cfg.SisterLemma -> SisterLemma
        { prevTag = sisMwe
        , currTag = curMwe
        , prevLemma = Cupt.lemma sisTok
        , currLemma = Cupt.lemma curTok
        }
      Cfg.SisterLemmaSister -> SisterLemmaSister
        { prevTag = sisMwe
        , currTag = curMwe
        , prevLemma = Cupt.lemma sisTok
        }
      Cfg.SisterLemmaCurrent -> SisterLemmaCurrent
        { prevTag = sisMwe
        , currTag = curMwe
        , currLemma = Cupt.lemma curTok
        }
      Cfg.SisterTagsOnly -> SisterTagsOnly
        { prevTag = sisMwe
        , currTag = curMwe
        }
      Cfg.SisterTagsAndDepRel -> SisterTagsAndDepRel
        { prevTag = sisMwe
        , prevRel = Cupt.deprel sisTok
        , currTag = curMwe
        , currRel = Cupt.deprel curTok
        }


----------------------------------------------
-- CRF element
----------------------------------------------


-- | Create a CRF training/tagging element.
mkElem
  :: (Ord mwe, Read mwe, Show mwe)
  => Cfg.FeatConfig
  -> AH.EncHype mwe Cupt.Token
  -> CRF.Elem IO (Map.RefMap IO) (Feat mwe) F.LogFloat
-- mkElem depTree =
mkElem cfg encoded =

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
      | isSister = collectSister
          (Cfg.sisterOptions cfg)
          (headTok, AH.origLabel headMWE)
          (tailTok, AH.origLabel tailMWE)
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
        headMWE = AH.encLabel encoded arcHead
        headTok = AH.encToken encoded (AH.encNode encoded arcHead)
        unary = collectUnary (Cfg.unaryOptions cfg) (headTok, AH.origLabel headMWE)
        binary = concat . maybeToList $ do
          parTok <- AH.encParent encoded arcHead
          parMwe <- AH.parentLabel headMWE
          return $ collectParent
            (Cfg.parentOptions cfg)
            (parTok, parMwe)
            (headTok, AH.origLabel headMWE)
      in
        unary ++ binary


----------------------------------------------
-- Training
----------------------------------------------


-- | A particular CRF model (IO embedded parameter map).
type ParaMap mwe = Map.RefMap IO (Feat mwe) Double


-- | Train disambiguation module.
train
  :: (Ord mwe, Read mwe, Show mwe)
  => Cfg.FeatConfig
  -> SGD.SgdArgs                  -- ^ SGD configuration
  -> [AH.DepTree mwe Cupt.Token]  -- ^ Training data
  -> [AH.DepTree mwe Cupt.Token]  -- ^ Development data
  -> IO (ParaMap mwe)
train cfg sgdArgsT trainData devData = do
  hSetBuffering stdout NoBuffering

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
  let fromSent = mkElem cfg . AH.encodeTree

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
  :: (Ord mwe, Read mwe, Show mwe)
  => Cfg.FeatConfig
  -> ParaMap mwe
  -> AH.DepTree mwe Cupt.Token
  -> Mame.Mame (Map.RefMap IO) (Feat mwe) F.LogFloat IO (AH.DepTree mwe Cupt.Token)
tagOne cfg paraMap depTree = do
  let encTree = AH.encodeTree depTree
      crfElem = mkElem cfg encTree
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
  :: (Ord mwe, Read mwe, Show mwe)
  => Cfg.FeatConfig
  -> ParaMap mwe
  -> [AH.DepTree mwe Cupt.Token]
  -> IO [AH.DepTree mwe Cupt.Token]
tagMany cfg paraMap depTrees = do
  let makeBuff = Mame.Make
        { Mame.mkValueBuffer = Map.newRefMap M.empty
        , Mame.mkDoubleBuffer = Map.newRefMap M.empty
        }
  Mame.runMame makeBuff $ do
    mapM (tagOne cfg paraMap) depTrees


-- -- | High-level tagging function.
-- tagFile
--   :: (ParaMap MWE)
--   -> FilePath -- ^ Input file
--   -> FilePath -- ^ Output file
--   -> IO ()
-- tagFile paraMap inpFile outFile = do
--   xs <- mapMaybe encodeCupt <$> Cupt.readCupt inpFile
--   ys <- tagMany paraMap xs
--   Cupt.writeCupt (map decodeCupt ys) outFile


-- | High-level tagging function.
tagFile
  :: S.Set Cupt.MweTyp
  -> Cfg.FeatConfig
  -> ParaMap MWE
  -> ParaMap (Maybe Cupt.MweTyp)
  -> FilePath -- ^ Input file
  -> FilePath -- ^ Output file
  -> IO ()
tagFile typSet cfg paraMap paraMap' inpFile outFile = do
  xs <- mapMaybe (encodeCupt . Cupt.decorate) <$> Cupt.readCupt inpFile
  ys <- map (encodeTypeAmbiguity typSet) <$> tagMany cfg paraMap xs
  zs <- tagMany cfg paraMap' ys
  Cupt.writeCupt (map (Cupt.abstract . decodeCupt') zs) outFile
