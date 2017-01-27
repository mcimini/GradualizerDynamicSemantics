sig fpl.

kind term type.
kind typ type.

type int typ.
type bool typ.
type arrow typ -> typ -> typ.
type list typ -> typ.
type sum typ -> typ -> typ.
type times typ -> typ -> typ.
type all (typ -> typ) -> typ.
type mu (typ -> typ) -> typ.
type succ term -> term.
type zero term.
type isZero term -> term.
type pred term -> term.
type ff term.
type tt term.
type if term -> term -> term -> term.
type abs typ -> (term -> term) -> term.
type app term -> term -> term.
type emptyList typ -> term.
type cons typ -> term -> term -> term.
type isnil typ -> term -> term.
type tail typ -> term -> term.
type head typ -> term -> term.
type inl typ -> term -> term.
type inr typ -> term -> term.
type case term -> (term -> term) -> (term -> term) -> term.
type pair term -> term -> term.
type fst term -> term.
type snd term -> term.
type absT (typ -> term) -> term.
type appT term -> typ -> term.
type fold term -> (typ -> typ) -> term.
type unfold term -> term.
type fix term -> term.
type let term -> (term -> term) -> term.
type letrec typ -> (term -> term) -> (term -> term) -> term.
type try term -> term -> term.
type raise typ -> term -> term.

type value term -> o.

type error term -> o.


type typeOf term -> typ -> o.

type step term -> term -> o.

type multistep term -> term -> o.

