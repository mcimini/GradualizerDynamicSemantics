sig stlc_if.

kind	term			type.
kind	typ				type.

type	bool			typ.
type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.

type		if		term -> term -> term -> term.
type		tt	term.
type		ff	term.

type	int			typ.
type	zero		term.
type	multistep			term -> term -> o.

