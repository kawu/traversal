{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- | Support for the Cupt format -- see http://multiword.sourceforge.net/PHITE.php?sitesig=CONF&page=CONF_04_LAW-MWE-CxG_2018&subpage=CONF_45_Format_specification.


module NLP.Departage.DepTree.Cupt
  (
    -- * Types
    GenSent
  , GenToken (..)
  , MaySent
  , MayToken
  , Sent
  , Token
  , TokID (..)
  , MweID
  , MweTyp
  , chosen

    -- * Parsing
  , readCupt
  , parseCupt
  , parsePar

    -- * Rendering
  , writeCupt
  , renderCupt
  , renderPar

    -- * Conversion
  , rootParID
  , decorate
  , preserveOnly
  , abstract
  ) where


-- import           Control.Arrow (second)

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List as List
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Text as T


-----------------------------------
-- Types
-----------------------------------


-- | Concrete types.
type MaySent = GenSent (Maybe MweTyp)
type MayToken = GenToken (Maybe MweTyp)
type Sent = GenSent MweTyp
type Token = GenToken MweTyp


-- | A Cupt element, i.e., a dependency tree with MWE annotations
type GenSent mwe = [GenToken mwe]


-- | See description of the CoNLL-U format: http://universaldependencies.org/format.html
data GenToken mwe = Token
  { tokID :: TokID
    -- ^ Word index, integer starting at 1 for each new sentence; may be a range
    -- for multiword tokens; may be a decimal number for empty nodes.
  , orth :: T.Text
    -- ^ Orthographic form
  , lemma :: T.Text
  , upos :: T.Text
    -- ^ Universal POS
  , xpos :: T.Text
    -- ^ Language-specific POS
  , feats :: M.Map T.Text T.Text
    -- ^ List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.
  , dephead :: TokID
    -- ^ Head of the current word, which is either a value of ID or zero [0].
  , deprel :: T.Text
    -- ^ Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.
  , deps :: T.Text
    -- ^ Enhanced dependency graph in the form of a list of head-deprel pairs.
  , misc :: T.Text
    -- ^ Any other annotation. It does not seem to be used in Cupt, though?
  -- mwe :: [(MweID, Maybe MweTyp)]
  , mwe :: [(MweID, mwe)]
    -- ^ MWE-related annotation. It might be a list, i.e., the token can be a
    -- part of several MWEs. Note that only the first occurrence of an MWE is
    -- supplied with the `MweTyp`e.
  } deriving (Show, Eq, Ord)


-- | Word index, integer starting at 1 for each new sentence; may be a range for
-- multiword tokens.
--
-- WARNING: we use `TokIDRange 0 0` as a special value for tokens out of the
-- selected tokenization (basically it stands for '_').
data TokID
  = TokID Int
  | TokIDRange Int Int
  | TokIDCopy Int Int
    -- ^ An empty node (marked as `CopyOf` in UD data)
  deriving (Show, Eq, Ord)


-- | Sentence-local MWE ID.
type MweID = Int


-- | MWE type.
type MweTyp = T.Text


-----------------------------------
-- Parsing
-----------------------------------


-- | Is the token in the chosen segmentation?
chosen :: GenToken mwe -> Bool
chosen tok = upos tok /= "_"


-- | Read an entire Cupt file.
readCupt :: FilePath -> IO [[MaySent]]
readCupt = fmap parseCupt . L.readFile


-- | Parse an entire Cupt file.
parseCupt :: L.Text -> [[MaySent]]
parseCupt
  = map parsePar
  . filter (not . L.null)
  . L.splitOn "\n\n"


-- | Parse a given textual representation of a paragraph. It can be assumed to
-- contain no empty lines, but it can contain comments. Moreover, it can contain
-- several sentences, each starting with token ID == 1.
parsePar :: L.Text -> [MaySent]
parsePar
  = groupBy tokLE
  . map (parseToken . L.toStrict)
  . filter (not . L.isPrefixOf "#")
  . L.lines
  where
    tokLE tx ty = fstPos (tokID tx) <= fstPos (tokID ty)
    fstPos (TokID x) = x
    fstPos (TokIDRange x _) = x
    fstPos (TokIDCopy x _) = x
    -- tokDiff tx ty = tokID tx /= tokID ty


parseToken :: T.Text -> MayToken
parseToken line =
  case T.splitOn "\t" line of
    [id', orth', lemma', upos', xpos', feats', head', deprel', deps', misc', mwe'] -> Token
      { tokID = parseTokID id'
      , orth = orth'
      , lemma = lemma'
      , upos = upos'
      , xpos = xpos'
      , feats = parseFeats feats'
      , dephead = parseTokID head'
      , deprel = deprel'
      , deps = deps'
      , misc = misc'
      , mwe = parseMWE mwe'
      }
    _ -> error "Cupt.parseToken: incorrenct number of line elements"


-- | NOTE: We treat "-" as root ID, for the lack of a better solution (SL).
parseTokID :: T.Text -> TokID
parseTokID "_" = TokIDRange 0 0
parseTokID "-" = TokID 0
parseTokID txt
  | T.isInfixOf "-" txt =
      case map (read . T.unpack) . T.split (=='-') $ txt of
        [x, y] -> TokIDRange x y
        _ -> error "Cupt.parseTokID: invalid token ID with -"
  | T.isInfixOf "." txt =
      case map (read . T.unpack) . T.split (=='.') $ txt of
        [x, y] -> TokIDCopy x y
        _ -> error "Cupt.parseTokID: invalid token ID with ."
  | otherwise =
      TokID $ read (T.unpack txt)


parseFeats :: T.Text -> M.Map T.Text T.Text
parseFeats txt =
  case txt of
    "_" -> M.empty
    _ -> M.fromList
      . map toPair
      . map (T.splitOn "=")
      . T.splitOn "|"
      $ txt
  where
    toPair [x, y] = (x, y)
    toPair [x] = (x, "")
    toPair _ = error "Cupt.parseFeats.toPair: not a pair!"


parseMWE :: T.Text -> [(MweID, Maybe MweTyp)]
parseMWE txt =
  case txt of
    "*" -> [] -- no MWE
    "_" -> [] -- underspecified, we represent it by [] too
    _ -> map parseOneMWE . T.splitOn ";" $ txt
  where
    parseOneMWE x =
      case T.splitOn ":" x of
        [mid] -> (read $ T.unpack mid, Nothing)
        [mid, mty] -> (read $ T.unpack mid, Just mty)
        _ -> error "Cupt.parseMWE: ???"


-----------------------------------
-- Rendering
-----------------------------------


writeCupt :: [[MaySent]] -> FilePath -> IO ()
writeCupt xs filePath = L.writeFile filePath (renderCupt xs)


renderCupt :: [[MaySent]] -> L.Text
renderCupt = L.intercalate "\n" . map renderPar


renderPar :: [MaySent] -> L.Text
renderPar = L.unlines . map renderToken . concat


renderToken :: MayToken -> L.Text
renderToken Token{..} =
  L.intercalate "\t"
  [ renderTokID tokID
  , L.fromStrict orth
  , L.fromStrict lemma
  , L.fromStrict upos
  , L.fromStrict xpos
  , renderFeats feats
  , renderTokID dephead
  , L.fromStrict deprel
  , L.fromStrict deps
  , L.fromStrict misc
  , renderMWE mwe
  ]


renderTokID :: TokID -> L.Text
renderTokID tid =
  case tid of
    TokID x ->
      psh x
    TokIDRange 0 0 ->
      "_"
    TokIDRange x y ->
      L.intercalate "-" [psh x, psh y]
    TokIDCopy x y ->
      L.intercalate "." [psh x, psh y]
  where
    psh = L.pack . show


renderFeats :: M.Map T.Text T.Text -> L.Text
renderFeats featMap
  | M.null featMap = "_"
  | otherwise = L.intercalate "|" . map renderPair $ M.toList featMap
  where
    renderPair (att, val) = L.concat
      [ L.fromStrict att
      , "="
      , L.fromStrict val
      ]


renderMWE :: [(MweID, Maybe MweTyp)] -> L.Text
renderMWE xs
  | null xs = "*"
  | otherwise = L.intercalate ";" . map renderMwePart $ xs
  where
    renderMwePart (mweID, mayTyp) =
      case mayTyp of
        Nothing -> renderMweID mweID
        Just tp -> L.intercalate ":"
          [ renderMweID mweID
          , L.fromStrict tp ]
    renderMweID = L.pack . show


-----------------------------------
-- Conversion
-----------------------------------


-- | An artificial root token.
root :: Token
root = Token
  { tokID = TokID 0
  , orth = ""
  , lemma = ""
  , upos = ""
  , xpos = ""
  , feats = M.empty
  , dephead = rootParID
  , deprel = ""
  , deps = ""
  , misc = ""
  , mwe = []
  }


-- | ID to refer to the parent of the artificial root node.
rootParID :: TokID
rootParID = TokID (-1)


-- | Decorate all MWE instances with their types and add the artificial root node.
decorate :: MaySent -> Sent
decorate =
  (root:) . snd . List.mapAccumL update M.empty
  where
    update typMap tok =
      let (typMap', mwe') = List.mapAccumL updateOne typMap (mwe tok)
      in  (typMap', tok {mwe=mwe'})
    updateOne typMap (mweID, mweTyp) =
      case mweTyp of
        Nothing  -> (typMap, (mweID, typMap M.! mweID))
        Just typ -> (M.insert mweID typ typMap, (mweID, typ))


-- | Preserve only selected MWE annotations.
preserveOnly :: MweTyp -> Sent -> Sent
preserveOnly mweTyp =
  map update
  where
    update tok = tok {mwe = filter preserve (mwe tok)}
    preserve (_, typ) = typ == mweTyp


-- | Inverse of `decorate`.
abstract :: Sent -> MaySent
abstract =
  snd . List.mapAccumL update S.empty . tail
  where
    update idSet tok =
      let (idSet', mwe') = List.mapAccumL updateOne idSet (mwe tok)
      in  (idSet', tok {mwe=mwe'})
    updateOne idSet (mweID, mweTyp) =
      if S.member mweID idSet
      then (idSet, (mweID, Nothing))
      else (S.insert mweID idSet, (mweID, Just mweTyp))


-----------------------------------
-- Utils
-----------------------------------


-- | A version of `List.groupBy` which always looks at the adjacent elements.
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy _ [x] = [[x]]
groupBy eq (x : y : rest)
  | eq x y = addHD x $ groupBy eq (y : rest)
  | otherwise = [x] : groupBy eq (y : rest)
  where
    addHD x (xs : xss) = (x : xs) : xss
    addHD x [] = [[x]]
