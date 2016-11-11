sig stlc_unit.

kind	term			type.
kind	typ				type.

type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.

type 	unitType 	typ.
type	unit		term.

type	int			typ.
type	zero		term.
type	multistep			term -> term -> o.

