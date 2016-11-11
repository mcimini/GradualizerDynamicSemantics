sig stlc_integers.

kind	term			type.
kind	typ				type.

type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.
type 	succ 		term -> term.
type 	zero 		term.
type 	pred 		term -> term.
type 	error 		term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.

type	int			typ.
type	zero		term.
type	multistep			term -> term -> o.
