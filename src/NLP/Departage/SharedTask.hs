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
  -- , decodeCupt
  , encodeTypeAmbiguity
  , retrieveTypes
  -- , retrieveTypes'
  , trainSimple
  , trainEnsemble
  , TagConfig(..)
  , tagEnsemble
  , tagSimple

  -- * Utils
  , liftCase
  , liftDolD
  , removeDeriv
  , copyLemma
  , copyUpos1
  , removeMweAnnotations
  , depRelStats
  , readDataWith
  ) where


import           GHC.Generics (Generic)

import           Control.Applicative ((<|>))
import           Control.Monad (guard, forM_, forM, when, unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State.Strict as State
-- import qualified Pipes as Pipes

import           System.IO (hSetBuffering, stdout, BufferMode (..))

import           Data.Ord (comparing)
import           Data.Maybe (listToMaybe, maybeToList, mapMaybe, catMaybes)
import qualified Data.Foldable as Fold
import           Data.Semigroup (Max(..))
import qualified Data.List as L
import qualified Data.Tree as R
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LT
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

import           NLP.Departage.Core (MWE, Feat(..), ParaMap)

-- import qualified NLP.Departage.FeatConfig as Cfg
import qualified NLP.Departage.Config as Cfg
import qualified NLP.Departage.Config.Feat as Cfg
import qualified NLP.Departage.Model as Model

import Debug.Trace (trace)


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


-- -- | Encode a Cupt sentence as a "sequential" dependency tree, i.e., a
-- -- dependency tree which is structurally equivalent to a sequence.
-- encodeCuptSeq :: Cupt.Sent -> Maybe (AH.DepTree MWE Cupt.Token)
-- encodeCuptSeq = mkT
--
--   where
--
--     mkT toks =
--       case toks of
--         [] -> Nothing
--         tok : rest -> Just $ node (tokLabel tok) (goF rest)
--
--     goF toks =
--       case toks of
--         [] -> []
--         tok : rest -> catMaybes
--           [ Just $ leaf (tokLabel tok)
--           , mkT rest
--           ]
--
--     tokLabel tok = (, tok) $
--       if Cupt.mwe tok == []
--       then P.fromList [(True, 0.0), (False, 1.0)]
--       else P.fromList [(True, 1.0), (False, 0.0)]
--
--     node label subTrees = R.Node
--       { R.rootLabel = label
--       , R.subForest = subTrees
--       }
--     leaf label = node label []


-- | Encode a Cupt sentence as a "sequential" dependency tree, i.e., a
-- dependency tree which is structurally equivalent to a sequence.
encodeCuptSeq :: Cupt.Sent -> Maybe (AH.DepTree MWE Cupt.Token)
encodeCuptSeq sent =

  case mkT sent of
    [depTree] -> Just depTree
    _ -> trace "SharedTask.encodeCuptSeq: sempty sentence?" Nothing

  where

    mkT toks =
      case toks of
        tok : rest -> [node (tokLabel tok) (mkT rest)]
        [] -> []

    tokLabel tok = (, tok) $
      if Cupt.mwe tok == []
      then P.fromList [(True, 0.0), (False, 1.0)]
      else P.fromList [(True, 1.0), (False, 0.0)]

    node label subTrees = R.Node
      { R.rootLabel = label
      , R.subForest = subTrees
      }
    leaf label = node label []


-- -- | Decode a Cupt sentence from a dependency tree.
-- decodeCupt :: AH.DepTree MWE Cupt.Token -> Cupt.Sent
-- decodeCupt =
--   L.sortBy (comparing position) . R.flatten . fmap decodeTok
--   where
--     position tok = case Cupt.tokID tok of
--       Cupt.TokID x -> (x, -1)
--       Cupt.TokIDRange x y -> (x, x - y)
--     decodeTok (prob, tok) =
--       case M.lookup True (P.unProb prob) of
--         Just p  ->
--           if p > 0.5
--           then tok {Cupt.mwe = [(1, T.pack $ "MWE:" ++ show p)]}
--           else tok {Cupt.mwe = []}
--         Nothing -> tok {Cupt.mwe = []}
-- --       case M.lookupMax (P.unProb prob) of
-- --         Just (True, _) -> tok {Cupt.mwe = [(0, Just "MWE")]}
-- --         _ -> tok {Cupt.mwe = []}


-- | Decode a Cupt sentence from a dependency tree.
-- Does not overwrite the existing annotations.
decodeCupt
  :: AH.DepTree (Maybe Cupt.MweTyp) Cupt.Token
  -> Cupt.Sent
decodeCupt inputTree
  = L.sortBy (comparing position)
  . concatMap R.flatten
  . flip State.evalState (maxMweID inputTree + 1)
  . decodeTree
  $ inputTree
  where
    position tok = case Cupt.tokID tok of
      Cupt.TokID x -> (x, -1)
      Cupt.TokIDRange x y -> (x, x - y)
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
          subTrees' <- decode (Just mwe) Nothing   subTrees
          forest'   <- decode  parent   (Just mwe) forest
          -- let root' = tok {Cupt.mwe = [mwe]}
          -- DO NOT OVERWRITE ANNOTATION!
          let root' = tok {Cupt.mwe = mwe : Cupt.mwe tok}
              tree' = R.Node root' subTrees'
          return $ tree' : forest'
        _ -> do
          subTrees' <- decode Nothing Nothing subTrees
          forest'   <- decode parent  Nothing forest
          -- let root' = tok {Cupt.mwe = []}
          let root' = tok -- DO NOT OVERWRITE ANNOTATION!
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


-- | Determine the maximum mwe ID present in the given tree.
maxMweID :: AH.DepTree mwe Cupt.Token -> Int
maxMweID =
  getMax . Fold.foldMap (mweID . snd)
  where
    mweID tok = case Cupt.mwe tok of
      [] -> Max 0
      xs -> Max . maximum $ map fst xs



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
      Cfg.UnaryPos -> UnaryPos
        { currTag = mwe
        , currPos = Cupt.upos tok
        }
      Cfg.UnaryDepRel -> UnaryDepRel
        { currTag = mwe
        , currRel = Cupt.deprel tok
        }


-- | Collect binary parent features.
collectGrandpa
  :: S.Set Cfg.GrandpaOption
  -> (Cupt.Token, mwe) -- ^ Grandpa
  -> (Cupt.Token, mwe) -- ^ Current
  -> [Feat mwe]
collectGrandpa options (graTok, graMwe) (curTok, curMwe) =
  map collect (S.toList options)
  where
    collect option = case option of
      Cfg.GrandpaOrth -> GrandpaOrth
        { granTag = graMwe
        , currTag = curMwe
        , granOrth = Cupt.orth graTok
        , currOrth = Cupt.orth curTok
        }
      Cfg.GrandpaLemma -> GrandpaLemma
        { granTag = graMwe
        , currTag = curMwe
        , granLemma = Cupt.lemma graTok
        , currLemma = Cupt.lemma curTok
        }
      Cfg.GrandpaTagsOnly -> GrandpaTagsOnly
        { granTag = graMwe
        , currTag = curMwe
        }
      Cfg.GrandpaTagsAndDepRel -> GrandpaTagsAndDepRel
        { granTag = graMwe
        , currTag = curMwe
        , currRel = Cupt.deprel curTok
        }
      Cfg.GrandpaUnordLemma ->
        if Cupt.lemma graTok <= Cupt.lemma curTok
        then BinaryUnordLemma
             { fstTag   = graMwe
             , fstLemma = Cupt.lemma graTok
             , sndTag   = curMwe
             , sndLemma = Cupt.lemma curTok }
        else BinaryUnordLemma
             { sndTag   = graMwe
             , sndLemma = Cupt.lemma graTok
             , fstTag   = curMwe
             , fstLemma = Cupt.lemma curTok }


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

      Cfg.ParentLemmaParentPosCurrentDepRel -> ParentLemmaParentPosCurrentDepRel
        { parentTag = parMwe
        , currTag = curMwe
        , parentLemma = Cupt.lemma parTok
        , currPos = Cupt.upos curTok
        , currRel = Cupt.deprel curTok
        }

      Cfg.ParentLemmaCurrentPosParentDepRel -> ParentLemmaCurrentPosParentDepRel
        { parentTag = parMwe
        , currTag = curMwe
        , parentPos = Cupt.upos parTok
        , currLemma = Cupt.lemma curTok
        , currRel = Cupt.deprel curTok
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
      Cfg.ParentUnordLemma ->
        if Cupt.lemma parTok <= Cupt.lemma curTok
        then BinaryUnordLemma
             { fstTag   = parMwe
             , fstLemma = Cupt.lemma parTok
             , sndTag   = curMwe
             , sndLemma = Cupt.lemma curTok }
        else BinaryUnordLemma
             { sndTag   = parMwe
             , sndLemma = Cupt.lemma parTok
             , fstTag   = curMwe
             , fstLemma = Cupt.lemma curTok }


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

      Cfg.SisterLemmaSisterPosCurrent -> SisterLemmaSisterPosCurrent
        { prevTag = sisMwe
        , currTag = curMwe
        , prevLemma = Cupt.lemma sisTok
        , currPos = Cupt.upos curTok
        }

      Cfg.SisterLemmaCurrentPosSister -> SisterLemmaCurrentPosSister
        { prevTag = sisMwe
        , currTag = curMwe
        , prevPos = Cupt.upos sisTok
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
      Cfg.SisterUnordLemma ->
        if Cupt.lemma sisTok <= Cupt.lemma curTok
        then BinaryUnordLemma
             { fstTag   = sisMwe
             , fstLemma = Cupt.lemma sisTok
             , sndTag   = curMwe
             , sndLemma = Cupt.lemma curTok }
        else BinaryUnordLemma
             { sndTag   = sisMwe
             , sndLemma = Cupt.lemma sisTok
             , fstTag   = curMwe
             , fstLemma = Cupt.lemma curTok }


----------------------------------------------
-- Feature extraction
----------------------------------------------


-- | Retrieve the list of features for the given sentence/arc pair.
featuresIn
  :: (Ord mwe)
  => Cfg.Config
  -> Cfg.FeatConfig
  -> AH.EncHype mwe Cupt.Token
  -> H.Arc -> [Feat mwe]
featuresIn globCfg cfg encoded =

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
      -- WARNING: if `sequential`, then `isSister` is meaningless!
      | isSister && not (Cfg.sequential globCfg) = collectSister
          (Cfg.unSet $ Cfg.sisterOptions cfg)
          (headTok, AH.origLabel headMWE)
          (tailTok, AH.origLabel tailMWE)
      | otherwise = do
          granTok <- maybeToList $ AH.encParent encoded arcHead
          granMWE <- maybeToList $ AH.parentLabel headMWE
          collectGrandpa
            (Cfg.unSet $ Cfg.grandpaOptions cfg)
            (granTok, granMWE)
            (tailTok, AH.origLabel tailMWE)
      where
        headMWE = AH.encLabel encoded arcHead
        headTok = AH.encToken encoded (AH.encNode encoded arcHead)
        tailMWE = AH.encLabel encoded arcTail
        tailTok = AH.encToken encoded (AH.encNode encoded arcTail)
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
  => Cfg.Config
  -> Cfg.FeatConfig
  -> AH.EncHype mwe Cupt.Token
  -- -> CRF.Elem IO (Map.RefMap IO) (Feat mwe) F.LogFloat
  -> CRF.Elem (Feat mwe) F.LogFloat
mkElem globCfg cfg encoded =
  CRF.Elem
  { CRF.elemHype = AH.encHype encoded
  , CRF.elemProb = F.logFloat <$> AH.encArcProb encoded
  , CRF.elemFeat = map (, 1) . featuresIn globCfg cfg encoded
--   , CRF.elemFeat = CRF.defaultFeat $ \arcID -> do
--       Map.newRefMap (featsIn arcID)
  }
  where
    -- featsIn = map (, 1) . featuresIn cfg encoded


----------------------------------------------
-- Training
----------------------------------------------


{-# SPECIALIZE sgd :: (Ord mwe, Hashable mwe) => SGD.SgdArgs -> (Int -> Mame.Mame (Map.IndiMap IO) (Feat mwe) F.LogFloat IO ()) -> (Map.IndiMap IO (Feat mwe) Double -> [AH.DepTree mwe Cupt.Token] -> Mame.Mame (Map.IndiMap IO) (Feat mwe) F.LogFloat IO ()) -> SGD.Dataset.Dataset (AH.DepTree mwe Cupt.Token) -> Map.IndiMap IO (Feat mwe) Double -> Mame.Mame (Map.IndiMap IO) (Feat mwe) F.LogFloat IO () #-}

{-# SPECIALIZE gradOn :: (Ord mwe, Hashable mwe) => [(CRF.Elem (Feat mwe) F.LogFloat, H.Arc -> F.LogFloat)] -> Map.IndiMap IO (Feat mwe) Double -> Mame.Mame (Map.IndiMap IO) (Feat mwe) F.LogFloat IO () #-}


-- | Train disambiguation module.
train
  :: (Ord mwe, Read mwe, Show mwe, Hashable mwe)
  => Cfg.Config
  -> Cfg.FeatConfig
  -> SGD.SgdArgs                  -- ^ SGD configuration
  -> [AH.DepTree mwe Cupt.Token]  -- ^ Training data
  -> [AH.DepTree mwe Cupt.Token]  -- ^ Development data
  -> IO (ParaMap mwe)
train globCfg cfg sgdArgsT trainData devData = do
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
        featuresIn globCfg cfg enc arc
      featMap () = M.fromList . map (,0) $ S.toList featSet

  -- parameter map
  paraMap <- Map.fromMap $ featMap ()
  -- paraMap <- Map.newRefMap M.empty

  -- transforming data to the CRF form
  let fromSent = mkElem globCfg cfg . AH.encodeTree

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


-- | Train an ensemble MWE recognition model.
trainEnsemble
  :: Cfg.Config
  -> FilePath          -- ^ Training data
  -> Maybe FilePath    -- ^ Development data
  -> IO Model.EnsembleModel
trainEnsemble config trainPath devPath = do

  putStrLn "# Basic model"
  let readData = readDataWith config Nothing . Just
  trainData <- readData trainPath
  devData   <- case devPath of
    Nothing -> return []
    Just path -> readData path
  paraMap <- train config (Cfg.baseFeatConfig config) sgdCfg trainData devData

  putStrLn "# MWE identification model"
  typSet <- retrieveTypes . map Cupt.decorate <$> Cupt.readCupt trainPath
  let readDataMWE
        = fmap (map (encodeTypeAmbiguity typSet))
        . readDataWith config Nothing . Just
  trainDataMWE <- readDataMWE trainPath
  devDataMWE <- case devPath of
    Nothing -> return []
    Just path -> readDataMWE path
  paraMapMWE <- train config (Cfg.mweFeatConfig config) sgdCfg trainDataMWE devDataMWE

  return $ Model.EnsembleModel
    { Model.paraMapBase = paraMap
    , Model.paraMapMwe = paraMapMWE
    , Model.mweTypSet = typSet
    }


-- | Train a separate recognition model for a given MWE type.
trainSimple
  :: Cfg.Config
  -> Cupt.MweTyp       -- ^ Selected type
  -> FilePath          -- ^ Training data
  -> Maybe FilePath    -- ^ Development data
  -> IO Model.SimpleModel
trainSimple config mweTyp trainPath devPath = do

  putStrLn "# Identify MWE types"
  typSet <- retrieveTypes . map Cupt.decorate <$> Cupt.readCupt trainPath

  unless (S.member mweTyp typSet) $ do
    error "Provided type not in the set of available types!"

  putStrLn $ "# Training model for: " ++ T.unpack mweTyp
  let readData = readDataWith config (Just mweTyp) . Just
  trainData <- readData trainPath
  devData   <- case devPath of
    Nothing -> return []
    Just path -> readData path
  train config (Cfg.baseFeatConfig config) sgdCfg trainData devData


-- | Default SGD configuration.
sgdCfg :: SGD.SgdArgs
sgdCfg = SGD.sgdArgsDefault
  { SGD.iterNum=20
  , SGD.regVar=10.0
  , SGD.gamma=0.9
  , SGD.tau=5
  , SGD.gain0=0.1
  , SGD.batchSize=30
  }


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
  => Cfg.Config
  -> Cfg.FeatConfig
  -> TagConfig
  -> ParaMap mwe
  -> AH.DepTree mwe Cupt.Token
  -- -> Mame.Mame (Map.RefMap IO) (Feat mwe) F.LogFloat IO (AH.DepTree mwe Cupt.Token)
  -> Mame.Mame (Map.IndiMap IO) (Feat mwe) F.LogFloat IO (AH.DepTree mwe Cupt.Token)
tagOne globCfg cfg TagConfig{..} paraMap depTree = do
  let encTree = AH.encodeTree depTree
      crfElem = mkElem globCfg cfg encTree
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
  => Cfg.Config
  -> Cfg.FeatConfig
  -> TagConfig
  -> ParaMap mwe
  -> [AH.DepTree mwe Cupt.Token]
  -> IO [AH.DepTree mwe Cupt.Token]
tagMany globCfg cfg tagCfg paraMap depTrees = do
  featSet <- M.keysSet <$> Map.toMap paraMap
  let featMap () = M.fromList . map (,0) $ S.toList featSet
      makeBuff = Mame.Make
        { Mame.mkValueBuffer = Map.fromMap $ featMap ()
        , Mame.mkDoubleBuffer = Map.fromMap $ featMap ()
        }
  Mame.runMame makeBuff $ do
    mapM (tagOne globCfg cfg tagCfg paraMap) depTrees


-- | High-level tagging function.
tagEnsemble
  :: Cfg.Config
  -> TagConfig
  -> Model.EnsembleModel
--   -> FilePath -- ^ Input file
--   -> FilePath -- ^ Output file
  -> IO ()
tagEnsemble config tagConfig model = do -- inpFile outFile = do
  xs <- readDataWith config Nothing Nothing -- inpFile
  ys <- map (encodeTypeAmbiguity $ Model.mweTypSet model)
    <$> tagMany config (Cfg.baseFeatConfig config) tagConfig (Model.paraMapBase model) xs
  zs <- tagMany config (Cfg.mweFeatConfig  config) tagConfig (Model.paraMapMwe model) ys
  LT.putStrLn $ Cupt.renderCupt (map (Cupt.abstract . decodeCupt) zs) -- outFile


-- | High-level tagging function.
tagSimple
  :: Cfg.Config
  -> TagConfig
  -> Cupt.MweTyp
              -- ^ MWE type to focus on
  -> Model.SimpleModel
--   -> FilePath -- ^ Input file
--   -> FilePath -- ^ Output file
  -> IO ()
tagSimple config tagConfig mweTyp model = do -- inpFile outFile = do
  xs <- readDataWith config Nothing Nothing -- inpFile
  ys <- map (fmap addType)
    <$> tagMany config (Cfg.baseFeatConfig config) tagConfig model xs
  LT.putStrLn . Cupt.renderCupt $ map (Cupt.abstract . decodeCupt) ys -- outFile
  where
    addType (probMap, tok) = (,tok) . P.fromList $ do
      (mwe, prob) <- P.toList probMap
      return $
        if mwe
        then (Just mweTyp, prob)
        else (Nothing, prob)


-- -- | High-level tagging function with separate models.
-- tagSeparate
--   :: Cfg.Config
--   -> TagConfig
--   -> Model.SeparateModel
--   -> FilePath -- ^ Input file
--   -> FilePath -- ^ Output file
--   -> IO ()
-- tagSeparate config tagConfig model inpFile outFile = do
--   xs <- readDataWith config Nothing inpFile
--
--   let tagger = tagWithMany $ do
--         mweTyp <- S.toList $ Model.mweTypSet model
--         let paraMap = Model.paraMaps model M.! mweTyp
--         return $ tagMany (Cfg.baseFeatConfig config) tagConfig
--
--   ys <- map (encodeTypeAmbiguity $ Model.mweTypSet model)
--     <$> tagMany (Cfg.baseFeatConfig config) tagConfig (Model.paraMapBase model)  xs
--   zs <- tagMany (Cfg.mweFeatConfig  config) tagConfig (Model.paraMapMwe model) ys
--   Cupt.writeCupt (map (Cupt.abstract . decodeCupt) zs) outFile
--
--   where
--
--     tagWithMany taggers xs =
--       case taggers of
--         [] -> xs
--         tag : rest -> foldThem rest (tag xs)


----------------------------------------------
----------------------------------------------
-- Pre-processing, utils
----------------------------------------------
----------------------------------------------


-- | Read Cupt data from a given file (or <stdin> if `Nothing`).
readDataWith
  :: Cfg.Config
  -> Maybe Cupt.MweTyp
    -- ^ Ignore MWEs of all other types (if supplied)
  -> Maybe FilePath
  -> IO [AH.DepTree MWE Cupt.Token]
readDataWith config mayMweTyp mayPath
  = fmap ( mapMaybe
           $ encodeIt
           . discardOtherMWEs
           . Cupt.decorate
           -- . handleCase
         )
  $ case mayPath of
      Just path -> Cupt.readCupt path
      Nothing -> Cupt.parseCupt <$> LT.getContents
  where
    -- TODO: Cannot be done here when simple models are used!
    -- Probably even with the ensemble model this is wrong!
    -- handleCase =
    --   if Cfg.liftCase config
    --   then liftCase
    --   else id
    discardOtherMWEs =
      case mayMweTyp of
        Nothing -> id
        Just mweTyp -> Cupt.preserveOnly mweTyp
    encodeIt =
      if Cfg.sequential config
      then encodeCuptSeq
      else encodeCupt


-- | Clear all MWE annotations.
removeMweAnnotations :: Cupt.GenSent mwe -> Cupt.GenSent mwe
removeMweAnnotations = map $ \tok -> tok {Cupt.mwe = []}


-- | Compute dependency relation statistics.
depRelStats :: [Cupt.GenSent mwe] -> M.Map Cupt.MweTyp Int
depRelStats
  = M.fromListWith (+)
  . map (,1)
  . map Cupt.deprel
  . concat


----------------------------------------------
-- Case lifting
----------------------------------------------


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


-- | Apply `liftToks` over tokens with "dol" function and "D..." XPOS tag.
liftDolD :: Cupt.GenSent mwe -> Cupt.GenSent mwe
liftDolD = liftToks $ \tok ->
  Cupt.deprel tok == "dol" &&
  T.isPrefixOf "D" (Cupt.xpos tok)


----------------------------------------------
-- DERIV removal (Turkish)
----------------------------------------------


-- | Remove the tokens satisfying the given predicate.
removeToks
  :: (Cupt.GenToken mwe -> Bool)
  -> Cupt.GenSent mwe
  -> Cupt.GenSent mwe
removeToks tokPred sent

  = map updateTok
  $ filter (not . tokPred) sent

  where

    updateTok tok = tok {Cupt.dephead = updatePar (Cupt.dephead tok)}

    updatePar parID
      | toRemove parID = updatePar (parent parID)
      | otherwise = parID

    toRemove tokID = S.member tokID . S.fromList $ do
      tok <- sent
      guard $ tokPred tok
      return $ Cupt.tokID tok

    parent tokID = maybe (Cupt.TokID 0) id $ M.lookup tokID parMap
    parMap = M.fromList $ do
      tok <- sent
      return (Cupt.tokID tok, Cupt.dephead tok)


-- | Apply `removeToks` over tokens which perform deriv function.
removeDeriv :: Cupt.GenSent mwe -> Cupt.GenSent mwe
removeDeriv = removeToks $ \tok -> Cupt.deprel tok == "DERIV"


----------------------------------------------
-- Copy lemma from orth (Turkish)
----------------------------------------------


-- | Copy orth to lemma where lemma is not given.
copyLemma
  :: Cupt.GenSent mwe
  -> Cupt.GenSent mwe
copyLemma =
  map updateTok
  where
    updateTok tok
      | Cupt.lemma tok == "_" = tok {Cupt.lemma = T.toLower (Cupt.orth tok)}
      | otherwise = tok


----------------------------------------------
-- Copy xpos to upos (Turkish)
----------------------------------------------


-- | Copy XPOS (first letter only!) to UPOS.
copyUpos1
  :: Cupt.GenSent mwe
  -> Cupt.GenSent mwe
copyUpos1 =
  let updateTok tok = tok {Cupt.upos = T.take 1 (Cupt.xpos tok)}
  in  map updateTok
