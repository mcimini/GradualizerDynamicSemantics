sig gradual_stlc_exc.


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



kind label	 type.

type dyn		typ.
type consistency		typ -> typ -> o.
type join2		typ -> typ -> typ -> o.
type contains		term -> term -> o.
type containsError		term -> term -> o.
type ground		typ -> o.
type cast		term -> typ -> label -> typ -> term.
type blame		typ -> label -> term.
type typeOfCC		term -> typ -> o.
type stepC		term -> term -> o.
type getGroundOf		typ -> typ -> o.
type sameGround		typ -> typ -> o.
