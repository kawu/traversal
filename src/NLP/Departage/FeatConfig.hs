


module NLP.Departage.FeatConfig
  ( FeatConfig (..)
  , UnaryOption (..)
  , ParentOption (..)
  , SisterOption (..)
  , loadConfig
  ) where


import qualified Data.Set as S


-- | Feature configuration.
data FeatConfig = FeatConfig
  { unaryOptions :: S.Set UnaryOption
  , parentOptions :: S.Set ParentOption
  , sisterOptions :: S.Set SisterOption
  } deriving (Show, Eq, Ord)


-- | Types of unary options.
data UnaryOption
  = UnaryOrth
    -- ^ Standard unary (orth, mwe) pair
  | UnaryLemma
  deriving (Read, Show, Eq, Ord)


-- | Types of binary parent/child options.
data ParentOption
  = ParentOrth
    -- ^ Standard (orth, mwe) pair for parent and child
  | ParentLemma
  | ParentLemmaParent
  | ParentLemmaCurrent
  | ParentTagsOnly
  | ParentTagsAndDepRel
  deriving (Read, Show, Eq, Ord)


-- | Types of binary sister/sister options.
data SisterOption
  = SisterOrth
    -- ^ Standard (orth, mwe) pair for sisters
  | SisterLemma
  | SisterLemmaSister
  | SisterLemmaCurrent
  | SisterTagsOnly
  | SisterTagsAndDepRel
  deriving (Read, Show, Eq, Ord)


-- | Load feature configuration.
loadConfig :: FilePath -> IO FeatConfig
loadConfig filePath = do
  contents <- readFile filePath
  case lines contents of
    [unaryLine, parentLine, sisterLine] -> return $ FeatConfig
      { unaryOptions = parseOptionLine unaryLine
      , parentOptions = parseOptionLine parentLine
      , sisterOptions = parseOptionLine sisterLine
      }
    _ -> error "FeatConfig: number of lines different than 3"


parseOptionLine :: (Read opt, Ord opt) => String -> S.Set opt
parseOptionLine = S.fromList . map read . words
