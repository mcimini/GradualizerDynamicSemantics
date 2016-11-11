sig stlc_exc.

kind	term			type.
kind	typ				type.

type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.
type	error			term -> o.

type		excType			typ.
type		raise			typ -> term -> term.
type		try				term -> term -> term.

type	int			typ.
type	zero		term.
type	multistep			term -> term -> o.

