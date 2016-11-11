sig gradual_fpl.


kind term type.
kind typ type.

type mu (typ -> typ) -> typ.
type all (typ -> typ) -> typ.
type list typ -> typ.
type sum typ -> typ -> typ.
type times typ -> typ -> typ.
type bool typ.
type int typ.
type arrow typ -> typ -> typ.
type fold term -> (typ -> typ) -> term.
type absT (typ -> term) -> term.
type cons typ -> term -> term -> term.
type emptyList typ -> term.
type inr typ -> term -> term.
type inl typ -> term -> term.
type pair term -> term -> term.
type ff term.
type tt term.
type succ term -> term.
type zero term.
type abs typ -> (term -> term) -> term.
type unfold term -> term.
type appT term -> typ -> term.
type isnil typ -> term -> term.
type tail typ -> term -> term.
type head typ -> term -> term.
type case term -> (term -> term) -> (term -> term) -> term.
type snd term -> term.
type fst term -> term.
type if term -> term -> term -> term.
type isZero term -> term.
type pred term -> term.
type app term -> term -> term.
type let term -> (term -> term) -> term.
type letrec typ -> (term -> term) -> (term -> term) -> term.
type fix term -> term.
type try term -> term -> term.
type raise typ -> term -> term.

type value term -> o.

type error term -> o.


type typeOf term -> typ -> o.

type step term -> term -> o.

type nstep term -> term -> o.

% context app 1[], 2[1].
% context if 1[].
% context succ 1[].
% context pred 1[].
% context isZero 1[].
% context pair 1[], 2[1].
% context fst 1[].
% context snd 1[].
% context inr 1[].
% context inl 1[].
% context case 1[].
% context cons 2[], 3[2].
% context head 2[].
% context tail 2[].
% context isnil 2[].
% context appT 1[].
% context fold 1[].
% context unfold 1[].
% context fix 1[].
% context try 1[].
% context raise 1[].
% variance arrow CONTRA COV.
% eliminator app 1.
% eliminator if 1.
% eliminator pred 1.
% eliminator isZero 1.
% eliminator fst 1.
% eliminator snd 1.
% eliminator case 1.
% eliminator head 2.
% eliminator tail 2.
% eliminator isnil 2.
% eliminator appT 1.
% eliminator unfold 1.


kind label	 type.

type dyn		typ.
type typeOfGr		term -> typ -> o.
type matchArrow		typ -> typ -> typ -> o.
type matchMu		typ -> (typ -> typ) -> o.
type matchAll		typ -> (typ -> typ) -> o.
type matchList		typ -> typ -> o.
type matchSum		typ -> typ -> typ -> o.
type matchTimes		typ -> typ -> typ -> o.
type matchBool		typ -> o.
type matchInt		typ -> o.
type matchDyn		typ -> o.
type flow		typ -> typ -> o.
type join2		typ -> typ -> typ -> o.
type static		typ -> o.
type contains		term -> term -> o.
type containsError		term -> term -> o.
type cast		term -> typ -> label -> typ -> term.
type blame		typ -> label -> term.
type typeOfCC		term -> typ -> o.
type stepC		term -> term -> o.
type ground		typ -> o.
type getGroundOf		typ -> typ -> o.
type sameGround		typ -> typ -> o.
type compToCC		term -> term -> typ -> o.

type result			term -> o. 
type subtype 		typ -> typ -> o.
type different		typ -> typ -> o.
type different_hi		typ -> typ -> o.
type isType 		typ -> o.
type flowAbs		typ -> typ -> o.
type lessPreciseType 		typ -> typ -> o.
type lessPrecise 		term -> term -> o.
type isType2 		typ -> o.
