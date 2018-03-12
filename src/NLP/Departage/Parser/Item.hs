{-# LANGUAGE TemplateHaskell #-}


-- | Chart item definition.


module NLP.Departage.Parser.Item
(
-- * Types
  Span (..)
, beg
, end

-- * Item
, Item (..)
, value
, span
) where


import           Prelude hiding (span)
import           Data.Lens.Light


--------------------------------------------------
-- Base types
--------------------------------------------------


-- | A position in the input sentence.
type Pos = Int


-- | A span over the input sentence.
data Span = Span {
    -- | The starting position.
      _beg   :: {-# UNPACK #-} !Pos
    -- | The ending position (or rather the position of the dot).
    , _end   :: {-# UNPACK #-} !Pos
    } deriving (Show, Eq, Ord)
$( makeLenses [''Span] )


-- | A chart item parametrized by the type of lexical objects stored
-- inside.
data Item a = Item
  { _span  :: {-# UNPACK #-} !Span
  , _value :: a
  } deriving (Show, Eq, Ord)
$( makeLenses [''Item] )
