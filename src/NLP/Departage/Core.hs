{-# LANGUAGE DeriveGeneric #-}


module NLP.Departage.Core
  ( MWE
  , Feat(..)
  , ParaMap
  ) where


import           GHC.Generics (Generic)

import qualified Data.Text as T
import           Data.Hashable (Hashable)

import qualified NLP.Departage.CRF.Map as Map


----------------------------------------------
-- Basic Types
----------------------------------------------


-- | Information about MWE
type MWE = Bool


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
  | ParentLemmaParentPosCurrentDepRel
    { parentTag :: mwe
    , currTag :: mwe
    , parentLemma :: T.Text
    , currPos :: T.Text
    , currRel :: T.Text
    }
  | ParentLemmaCurrentPosParentDepRel
    { parentTag :: mwe
    , currTag :: mwe
    , parentPos :: T.Text
    , currLemma :: T.Text
    , currRel :: T.Text
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
  | SisterLemmaSisterPosCurrent
    { prevTag :: mwe
    , currTag :: mwe
    , prevLemma :: T.Text
    , currPos :: T.Text
    }
  | SisterLemmaCurrentPosSister
    { prevTag :: mwe
    , currTag :: mwe
    , prevPos :: T.Text
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
  | GrandpaOrth
    { granTag :: mwe
    , currTag :: mwe
    , granOrth :: T.Text
    , currOrth :: T.Text
    }
  | GrandpaLemma
    { granTag :: mwe
    , currTag :: mwe
    , granLemma :: T.Text
    , currLemma :: T.Text
    }
  | GrandpaTagsOnly
    { granTag :: mwe
    , currTag :: mwe
    }
  | GrandpaTagsAndDepRel
    { granTag :: mwe
    , currTag :: mwe
    , currRel :: T.Text
    }
  | UnaryOrth
    { currTag :: mwe
    , currOrth :: T.Text
    }
  | UnaryLemma
    { currTag :: mwe
    , currLemma :: T.Text
    }
  | UnaryPos
    { currTag :: mwe
    , currPos :: T.Text
    }
  | UnaryDepRel
    { currTag :: mwe
    , currRel :: T.Text
      -- ^ Dependency type of the arc between the current node and its parent
    }
  | BinaryUnordLemma
    { fstTag :: mwe
    , sndTag :: mwe
    , fstLemma :: T.Text
    , sndLemma :: T.Text
    }
    -- ^ Binary feature which can refer to both parent and sister relation and
    -- in which the order of elements doesn't matter (i.e. it holds that
    -- `fstLemma <= sndLemma`).
  | BinaryTagsOnly
    { prevTag :: mwe
      -- ^ Tag corresponding to the previous node (sister or parent)
    , currTag :: mwe
      -- ^ Tag corresponding to the current node (sister or parent)
    }
  | BinaryTagsAndDepRel
    { prevTag :: mwe
    , currTag :: mwe
    , prevRel :: T.Text
      -- ^ Dependency type of the arc between the previous node and its parent
    , currRel :: T.Text
      -- ^ Dependency type of the arc between the current node and its parent
    }
  deriving (Generic, Read, Show, Eq, Ord)

instance Hashable mwe => Hashable (Feat mwe)


----------------------------------------------
-- Params
----------------------------------------------


-- | A particular CRF model (IO embedded parameter map).
type ParaMap mwe = Map.IndiMap IO (Feat mwe) Double
