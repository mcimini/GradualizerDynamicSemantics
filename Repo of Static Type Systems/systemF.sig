sig systemF.

kind	term			type.
kind	typ				type.

type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type 	all 			(typ -> typ) -> typ.
type    appT			term -> typ -> term.
type    absT			(typ -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.


type	int			typ.
type	zero		term.
type	multistep			term -> term -> o.
