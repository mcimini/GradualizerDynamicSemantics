sig stlc_lists.

kind	term			type.
kind	typ				type.

type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.

type	bool			typ.
type	list			typ -> typ.

type	tt		term.
type	ff		term.
type	emptyList	typ -> term.
type	isnil		typ -> term -> term. 
type	cons		typ -> term -> term -> term. 
type	head 		typ -> term -> term. 
type	tail 		typ -> term -> term. 

type	int			typ.
type	zero		term.
type	multistep			term -> term -> o.


