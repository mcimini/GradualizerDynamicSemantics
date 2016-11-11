sig recursive.

kind	term			type.
kind	typ				type.

type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type 	mu 			(typ -> typ) -> typ.
type 	fold 		term -> (typ -> typ) -> term.
type    unfold		term -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.

type	int			typ.
type	zero		term.
type	multistep			term -> term -> o.
