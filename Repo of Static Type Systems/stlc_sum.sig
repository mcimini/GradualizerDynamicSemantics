sig stlc_sum.

kind	term			type.
kind	typ				type.

type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.

type sum typ -> typ -> typ.

type		case 		term -> (term -> term) -> (term -> term) -> term.
type		inl			typ -> term -> term.
type		inr			typ -> term -> term.

type	int			typ.
type	zero		term.
type	multistep			term -> term -> o.

