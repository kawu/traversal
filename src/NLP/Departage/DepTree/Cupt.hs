-- See: http://multiword.sourceforge.net/PHITE.php?sitesig=CONF&page=CONF_04_LAW-MWE-CxG_2018&subpage=CONF_45_Format_specification


module NLP.Departage.DepTree.Cupt
  (
  ) where


import qualified Data.Text.Lazy as L
import qualified Data.Text as T


-- | A Cupt element, i.e., a dependency tree with MWE annotations
type Elem = [Token]


-- 1	O	o	ADP	prep:loc	AdpType=Prep|Case=Loc	2	case	_	_	*
-- 2	przyczynie	przyczyna	NOUN	subst:sg:loc:f	Case=Loc|Gender=Fem|Number=Sing	7	obl:arg	_	_	*
-- 3	śmierci	śmierć	NOUN	subst:sg:gen:f	Case=Gen|Gender=Fem|Number=Sing	2	nmod	_	_	*
-- 4	kolegi	kolega	NOUN	subst:sg:gen:m1	Animacy=Hum|Case=Gen|Gender=Masc|Number=Sing	3	nmod	_	_	*
-- 5	niewiele	niewiele	NUM	num:pl:acc:n2:rec	Case=Acc|Gender=Neut|NumForm=Word|Number=Plur	7	obj	_	_	*
-- 6	potrafią	potrafić	VERB	fin:pl:ter:imperf	Aspect=Imp|Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin|Voice=Act	0	root	0:root	_	*

-- | Also see the CoNLL-U format: http://universaldependencies.org/format.html
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
  , head :: TokID
    -- ^ Head of the current word, which is either a value of ID or zero [0].
  , deprel :: T.Text
    -- ^ Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.
  , deps :: T.Text
    -- ^ Enhanced dependency graph in the form of a list of head-deprel pairs.
  , misc :: T.Text
    -- ^ Any other annotation. It does not seem to be used in Cupt, though?
  } deriving (Show, Eq, Ord)


-- | Word index, integer starting at 1 for each new sentence; may be a range for
-- multiword tokens; may be a decimal number for empty nodes.
type TokID = [Int]


parseCupt :: L.Text -> [Elem]
