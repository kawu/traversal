let U = constructors ./unary.dhall in
let P = constructors ./parent.dhall in
let S = constructors ./sister.dhall in
let G = constructors ./grandpa.dhall in
{
  -- Feature templates related to the base model, which only identifies places where MWEs should occur
  baseFeatConfig =
    { unaryOptions = [U.UnaryLemma {=}]
    , parentOptions =
		[ P.ParentLemma {=}, P.ParentTagsOnly {=}, P.ParentTagsAndDepRel {=}, P.ParentUnordLemma {=}
		, P.ParentLemmaParentPosCurrentDepRel {=}, P.ParentLemmaCurrentPosParentDepRel {=}
		]
    , sisterOptions =
		[ S.SisterLemma {=}, S.SisterTagsOnly {=}, S.SisterTagsAndDepRel {=}, S.SisterUnordLemma {=}
		, S.SisterLemmaSisterPosCurrent {=}, S.SisterLemmaCurrentPosSister {=}
		]
	, grandpaOptions = [] : List ./grandpa.dhall
    }

  -- Feature templates related to the MWE model, which classifies MWE types 
, mweFeatConfig =
    { unaryOptions = [U.UnaryLemma {=}]
    , parentOptions =
		[ P.ParentLemma {=}, P.ParentTagsOnly {=}, P.ParentTagsAndDepRel {=}, P.ParentUnordLemma {=}
		, P.ParentLemmaParentPosCurrentDepRel {=}, P.ParentLemmaCurrentPosParentDepRel {=}
		]
    , sisterOptions =
		[ S.SisterLemma {=}, S.SisterTagsOnly {=}, S.SisterTagsAndDepRel {=}, S.SisterUnordLemma {=}
		, S.SisterLemmaSisterPosCurrent {=}, S.SisterLemmaCurrentPosSister {=}
		]
	, grandpaOptions = [] : List ./grandpa.dhall
    }

  -- Use the sequential encoding method instead of relying on the dependency trees?
, sequential = False
}
