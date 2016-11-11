sig stlc_tuples.

kind	term			type.
kind	typ				type.

type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.

type	tupleType		typ -> typ -> typ -> typ -> typ.
type	tuple			term -> term -> term -> term -> term.
type	select1				term -> term.
type	select2				term -> term.
type	select3				term -> term.
type	select4				term -> term.

type	int			typ.
type	zero		term.
type	multistep			term -> term -> o.

