{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


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
  , TagConfig(..)
  , tagFile

  -- * Utils
  , liftCase
  , readDataWith
  ) where


import           GHC.Generics (Generic)

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State.Strict as State
-- import qualified Pipes as Pipes

import           System.IO (hSetBuffering, stdout, BufferMode (..))

import           Data.Ord (comparing)
import           Data.Maybe (listToMaybe, maybeToList, mapMaybe)
import qualified Data.List as L
import qualified Data.Tree as R
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Hashable (Hashable)
-- import qualified Data.HashMap.Strict as HM

import qualified Data.Number.LogFloat as F
-- import qualified Data.PrimRef as Ref

import qualified NLP.Departage.Hype as H
import qualified NLP.Departage.DepTree.Cupt as Cupt
import qualified NLP.Departage.DepTree.AsHype as AH
import qualified NLP.Departage.Prob as P

import qualified NLP.Departage.CRF as CRF
import           NLP.Departage.CRF (gradOn)
import qualified NLP.Departage.CRF.SGD as SGD
import           NLP.Departage.CRF.SGD (sgd)
import qualified NLP.Departage.CRF.SGD.Dataset as SGD.Dataset
import qualified NLP.Departage.CRF.Map as Map
import qualified NLP.Departage.CRF.Mame as Mame

-- import qualified NLP.Departage.FeatConfig as Cfg
import qualified NLP.Departage.Config as Cfg
import qualified NLP.Departage.Config.Feat as Cfg

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
  -- L.sortBy (comparing position) . R.flatten . fmap decodeTok
  L.sortBy (comparing position) . concatMap R.flatten . flip State.evalState 1 . decodeTree
  where
    position tok = case Cupt.tokID tok of
      Cupt.TokID x -> (x, -1)
      Cupt.TokIDRange x y -> (x, x - y)
--     decodeTok (prob, tok) =
--       case listToMaybe . reverse . L.sortBy (comparing snd) $ M.toList (P.unProb prob) of
--         Just (Just typ, _) -> tok {Cupt.mwe = [(0, typ)]}
--         _ -> tok {Cupt.mwe = []}
    decodeTree tree = decode Nothing Nothing [tree]
    decode _ _ [] = return []
    decode parent sister (tree : forest) = do
      let (prob, tok) = R.rootLabel tree
          subTrees = R.subForest tree
      case listToMaybe . reverse . L.sortBy (comparing snd) $ M.toList (P.unProb prob) of
        Just (Just typ, _) -> do
          mwe <- case withPrev parent typ <|> withPrev sister typ of
            Nothing -> withNewID typ
            Just x  -> return x
              -- root' = tok {Cupt.mwe = [(0, typ)]}
          subTrees' <- decode (Just mwe) Nothing   subTrees
          forest'   <- decode  parent   (Just mwe) forest
          let root' = tok {Cupt.mwe = [mwe]}
              tree' = R.Node root' subTrees'
          return $ tree' : forest'
        _ -> do
          subTrees' <- decode Nothing Nothing subTrees
          forest'   <- decode parent  Nothing forest
          let root' = tok {Cupt.mwe = []}
              tree' = R.Node root' subTrees'
          return $ tree' : forest'
    withPrev prev typ = do
      (prevID, prevTyp) <- prev
      guard $ prevTyp == typ
      return (prevID, typ)
    withNewID typ = do
      k <- State.get
      State.put (k+1)
      return (k, typ)



-- -- | Add MWE identifiers so that adjacent (in the tree sense) MWEs have the same
-- -- ID, while non-adjacent MWEs are considered as different instances.
-- addMweIDsStupid :: Cupt.Sent -> Cupt.Sent
-- addMweIDsStupid sent =


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
  deriving (Generic, Read, Show, Eq, Ord)

instance Hashable mwe => Hashable (Feat mwe)


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
-- Feature extraction
----------------------------------------------


-- | Retrieve the list of features for the given sentence/arc pair.
featuresIn
  :: (Ord mwe)
  => Cfg.FeatConfig
  -> AH.EncHype mwe Cupt.Token
  -> H.Arc -> [Feat mwe]
featuresIn cfg encoded =

  \arcID ->
    let arcHead = H.head arcID hype
        arcTail = H.tail arcID hype
        featList =
          mkUnary arcHead ++
          concatMap (mkBinary arcHead) (S.toList arcTail)
    -- in M.fromList $ map (,1) featList
    in featList

  where

    hype =  AH.encHype encoded

    mkBinary arcHead arcTail
      | isSister = collectSister
          (Cfg.unSet $ Cfg.sisterOptions cfg)
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
        unary = collectUnary (Cfg.unSet $ Cfg.unaryOptions cfg) (headTok, AH.origLabel headMWE)
        binary = concat . maybeToList $ do
          parTok <- AH.encParent encoded arcHead
          parMwe <- AH.parentLabel headMWE
          return $ collectParent
            (Cfg.unSet $ Cfg.parentOptions cfg)
            (parTok, parMwe)
            (headTok, AH.origLabel headMWE)
      in
        unary ++ binary


----------------------------------------------
-- CRF element
----------------------------------------------


-- | Create a CRF training/tagging element.
mkElem
  :: (Ord mwe)
  => Cfg.FeatConfig
  -> AH.EncHype mwe Cupt.Token
  -- -> CRF.Elem IO (Map.RefMap IO) (Feat mwe) F.LogFloat
  -> CRF.Elem (Feat mwe) F.LogFloat
mkElem cfg encoded =
  CRF.Elem
  { CRF.elemHype = AH.encHype encoded
  , CRF.elemProb = F.logFloat <$> AH.encArcProb encoded
  , CRF.elemFeat = map (, 1) . featuresIn cfg encoded
--   , CRF.elemFeat = CRF.defaultFeat $ \arcID -> do
--       Map.newRefMap (featsIn arcID)
  }
  where
    -- featsIn = map (, 1) . featuresIn cfg encoded


----------------------------------------------
-- Training
----------------------------------------------


-- | A particular CRF model (IO embedded parameter map).
type ParaMap mwe = Map.IndiMap IO (Feat mwe) Double
-- type ParaMap mwe = Map.RefMap IO (Feat mwe) Double


{-# SPECIALIZE sgd :: (Ord mwe, Hashable mwe) => SGD.SgdArgs -> (Int -> Mame.Mame (Map.IndiMap IO) (Feat mwe) F.LogFloat IO ()) -> (Map.IndiMap IO (Feat mwe) Double -> [AH.DepTree mwe Cupt.Token] -> Mame.Mame (Map.IndiMap IO) (Feat mwe) F.LogFloat IO ()) -> SGD.Dataset.Dataset (AH.DepTree mwe Cupt.Token) -> Map.IndiMap IO (Feat mwe) Double -> Mame.Mame (Map.IndiMap IO) (Feat mwe) F.LogFloat IO () #-}

{-# SPECIALIZE gradOn :: (Ord mwe, Hashable mwe) => [(CRF.Elem (Feat mwe) F.LogFloat, H.Arc -> F.LogFloat)] -> Map.IndiMap IO (Feat mwe) Double -> Mame.Mame (Map.IndiMap IO) (Feat mwe) F.LogFloat IO () #-}


-- | Train disambiguation module.
train
  :: (Ord mwe, Read mwe, Show mwe, Hashable mwe)
  => Cfg.FeatConfig
  -> SGD.SgdArgs                  -- ^ SGD configuration
  -> [AH.DepTree mwe Cupt.Token]  -- ^ Training data
  -> [AH.DepTree mwe Cupt.Token]  -- ^ Development data
  -> IO (ParaMap mwe)
train cfg sgdArgsT trainData devData = do
  hSetBuffering stdout NoBuffering

  -- parameters, helpers
  let
    trainSize = length trainData
    batchSize = SGD.batchSize sgdArgsT

    doneTotal :: Int -> Int
    doneTotal = floor . done

    done :: Int -> Double
    done i
        = fromIntegral (i * batchSize)
        / fromIntegral trainSize

  -- list of features in the training dataset
  let featSet = S.fromList $ do
        sent <- trainData
        let enc = AH.encodeTree sent
        arc <- (S.toList . H.arcs) (AH.encHype enc)
        featuresIn cfg enc arc
      featMap () = M.fromList . map (,0) $ S.toList featSet

  -- parameter map
  paraMap <- Map.fromMap $ featMap ()
  -- paraMap <- Map.newRefMap M.empty

  -- transforming data to the CRF form
  let fromSent = mkElem cfg . AH.encodeTree

  -- enrich the dataset elements with potential functions, based on the current
  -- parameter values (and using `defaultPotential`)
  let withPhi xs = do
        paraPure <- liftIO $ Map.unsafeFreeze paraMap
        let crf x = F.logToLogFloat $ case paraPure x of
              Nothing -> 0
              Just v  -> v
        return $ do
          x@CRF.Elem{..} <- xs
          let phi = CRF.defaultPotential crf elemFeat
          return (x, phi)
--         forM xs $ \x@CRF.Elem{..} -> do
--           phi <- CRF.defaultPotential crf elemHype elemFeat
--           return (x, phi)

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
              Map.toMap paraMap >>= \m -> do
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
        gradOn batch grad
--         liftIO $ do
--           putStrLn "# GRADIENT"
--           gradMap <- Ref.readPrimRef $ Map.unRefMap grad
--           print gradMap

  -- Buffer creation
  let makeBuff = Mame.Make
        { Mame.mkValueBuffer = Map.fromMap $ featMap () -- Map.newRefMap M.empty
        , Mame.mkDoubleBuffer = Map.fromMap $ featMap () -- Map.newRefMap M.empty
        }

  -- pureTrainData <- trainData
  let pureTrainData = trainData
  SGD.Dataset.withVect pureTrainData $ \dataSet -> do
    -- TODO: change the type of `sgd` so that it runs internally `runMame` with
    -- provided `makeBuff`
    Mame.runMame makeBuff $ do
      sgd
        sgdArgsT
        notify
        computeGradient
        dataSet
        paraMap

  return paraMap


----------------------------------------------
-- Tagging
----------------------------------------------


-- | Additional tagging configuration.
data TagConfig = TagConfig
  { tagBestPath :: Bool
  }


-- | Tag the dependency tree given the model parameters.
--
-- NOTE: the input tree may have some MWE annotations. These will be ignored and
-- replaced by model-based annotations.
tagOne
  :: (Ord mwe, Read mwe, Show mwe, Hashable mwe)
  => Cfg.FeatConfig
  -> TagConfig
  -> ParaMap mwe
  -> AH.DepTree mwe Cupt.Token
  -- -> Mame.Mame (Map.RefMap IO) (Feat mwe) F.LogFloat IO (AH.DepTree mwe Cupt.Token)
  -> Mame.Mame (Map.IndiMap IO) (Feat mwe) F.LogFloat IO (AH.DepTree mwe Cupt.Token)
tagOne cfg TagConfig{..} paraMap depTree = do
  let encTree = AH.encodeTree depTree
      crfElem = mkElem cfg encTree
  -- START: computing arc potentials (UNSAFELY?)
  paraPure <- liftIO $ Map.unsafeFreeze paraMap
  let crf x = F.logToLogFloat $ case paraPure x of
        Nothing -> 0
        Just v  -> v
  -- phi <- CRF.defaultPotential crf (CRF.elemHype crfElem) (CRF.elemFeat crfElem)
      phi = CRF.defaultPotential crf (CRF.elemFeat crfElem)
  -- END: computing arc potentials
  let (prob, _) = CRF.marginals phi (AH.encHype encTree)
      nodeSet = CRF.bestPath phi (AH.encHype encTree)
      depTree' =
        if tagBestPath
        then AH.decodeTree' encTree nodeSet
        else AH.decodeTree encTree prob
  return depTree'


-- | Tag several dependency trees (using `tagOne`).
tagMany
  :: (Ord mwe, Read mwe, Show mwe, Hashable mwe)
  => Cfg.FeatConfig
  -> TagConfig
  -> ParaMap mwe
  -> [AH.DepTree mwe Cupt.Token]
  -> IO [AH.DepTree mwe Cupt.Token]
tagMany cfg tagCfg paraMap depTrees = do
  featSet <- M.keysSet <$> Map.toMap paraMap
  let featMap () = M.fromList . map (,0) $ S.toList featSet
      makeBuff = Mame.Make
        -- { Mame.mkValueBuffer = Map.newRefMap M.empty
        -- , Mame.mkDoubleBuffer = Map.newRefMap M.empty
        -- { Mame.mkValueBuffer = Map.newIndiMap $ Map.indiSet paraMap
        -- , Mame.mkDoubleBuffer = Map.newIndiMap $ Map.indiSet paraMap
        { Mame.mkValueBuffer = Map.fromMap $ featMap ()
        , Mame.mkDoubleBuffer = Map.fromMap $ featMap ()
        }
  Mame.runMame makeBuff $ do
    mapM (tagOne cfg tagCfg paraMap) depTrees


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
  -> Cfg.Config
  -> TagConfig
  -> ParaMap MWE
  -> ParaMap (Maybe Cupt.MweTyp)
  -> FilePath -- ^ Input file
  -> FilePath -- ^ Output file
  -> IO ()
tagFile typSet config tagConfig paraMap paraMap' inpFile outFile = do
  -- xs <- mapMaybe (encodeCupt . Cupt.decorate) <$> Cupt.readCupt inpFile
  xs <- readDataWith config inpFile
  ys <- map (encodeTypeAmbiguity typSet)
    <$> tagMany (Cfg.baseFeatConfig config) tagConfig paraMap  xs
  zs <- tagMany (Cfg.mweFeatConfig  config) tagConfig paraMap' ys
  Cupt.writeCupt (map (Cupt.abstract . decodeCupt') zs) outFile


-- -- | High-level tagging function.
-- tagFile
--   :: S.Set Cupt.MweTyp
--   -> Cfg.FeatConfig
--   -> ParaMap MWE
--   -> ParaMap (Maybe Cupt.MweTyp)
--   -> FilePath -- ^ Input file
--   -> FilePath -- ^ Output file
--   -> IO ()
-- tagFile typSet cfg paraMap paraMap' inpFile outFile = do
--   xs <- mapMaybe (encodeCupt . Cupt.decorate) <$> Cupt.readCupt inpFile
--   ys <- map (encodeTypeAmbiguity typSet) <$> tagMany cfg paraMap xs
--   zs <- tagMany cfg paraMap' ys
--   Cupt.writeCupt (map (Cupt.abstract . decodeCupt') zs) outFile


----------------------------------------------
-- Pre-processing
----------------------------------------------


-- | Read Cupt data from a given file.
readDataWith
  :: Cfg.Config
  -> FilePath
  -> IO [AH.DepTree MWE Cupt.Token]
readDataWith config
  = fmap (mapMaybe $ encodeCupt . Cupt.decorate . handleCase)
  . Cupt.readCupt
  where
    handleCase =
      if Cfg.liftCase config
      then liftCase
      else id


-- | Lift tokens satisfying the given predicate to a higher level (i.e., they
-- get attached to their grandparents).
liftToks
  :: (Cupt.GenToken mwe -> Bool)
  -> Cupt.GenSent mwe
  -> Cupt.GenSent mwe
liftToks tokPred sent =
  map liftIt sent
  where
    liftIt tok =
      if tokPred tok
      then tok {Cupt.dephead = parent (Cupt.dephead tok)}
      else tok
    parent tokID = maybe tokID id $ M.lookup tokID parMap
    parMap = M.fromList $ do
      tok <- sent
      return (Cupt.tokID tok, Cupt.dephead tok)


-- | Apply `liftToks` over tokens which perform the function of case markers.
liftCase :: Cupt.GenSent mwe -> Cupt.GenSent mwe
liftCase = liftToks $ \tok -> Cupt.deprel tok == "case"

