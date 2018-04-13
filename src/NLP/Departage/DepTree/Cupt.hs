{-# LANGUAGE OverloadedStrings #-}


-- | Support for the Cupt format -- see http://multiword.sourceforge.net/PHITE.php?sitesig=CONF&page=CONF_04_LAW-MWE-CxG_2018&subpage=CONF_45_Format_specification.


module NLP.Departage.DepTree.Cupt
  (
    -- * Types
    Sent
  , Token(..)
  , TokID
  , MweID
  , MweTyp

    -- * Parsing
  , readCupt
  , parseCupt
  , parseSent
  ) where


import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Text as T


-- | A Cupt element, i.e., a dependency tree with MWE annotations
type Sent = [Token]


-- | See description of the CoNLL-U format: http://universaldependencies.org/format.html
data Token = Token
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
  , topHead :: TokID
    -- ^ Head of the current word, which is either a value of ID or zero [0].
  , deprel :: T.Text
    -- ^ Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.
  , deps :: T.Text
    -- ^ Enhanced dependency graph in the form of a list of head-deprel pairs.
  , misc :: T.Text
    -- ^ Any other annotation. It does not seem to be used in Cupt, though?
  , mwe :: [(MweID, Maybe MweTyp)]
    -- ^ MWE-related annotation. It might be a list, i.e., the token can be a
    -- part of several MWEs. Note that only the first occurrence of an MWE is
    -- supplied with the `MweTyp`e.
  } deriving (Show, Eq, Ord)


-- | Word index, integer starting at 1 for each new sentence; may be a range for
-- multiword tokens; may be a decimal number for empty nodes.
type TokID = [Int]


-- | Sentence-local MWE ID.
type MweID = Int


-- | MWE type.
type MweTyp = T.Text


-- | Read an entire Cupt file.
readCupt :: FilePath -> IO [Sent]
readCupt = fmap parseCupt . L.readFile


-- | Parse an entire Cupt file.
parseCupt :: L.Text -> [Sent]
parseCupt
  = map parseSent
  . filter (not . L.null)
  . L.splitOn "\n\n"


-- | Parse a given textual representation of a sentence. It can be assumed to
-- contain no empty lines, but it can contain comments.
parseSent :: L.Text -> Sent
parseSent
  = map (parseToken . L.toStrict)
  . filter (not . L.isPrefixOf "#")
  . L.lines


parseToken :: T.Text -> Token
parseToken line =
  case T.splitOn "\t" line of
    [id', orth', lemma', upos', xpos', feats', head', deprel', deps', misc', mwe'] -> Token
      { tokID = parseTokID id'
      , orth = orth'
      , lemma = lemma'
      , upos = upos'
      , xpos = xpos'
      , feats = parseFeats feats'
      , topHead = parseTokID head'
      , deprel = deprel'
      , deps = deps'
      , misc = misc'
      , mwe = parseMWE mwe'
      }
    _ -> error "Cupt.parseToken: incorrenct number of line elements"


parseTokID :: T.Text -> TokID
parseTokID = map (read . T.unpack) . T.split (=='-')


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
