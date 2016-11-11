sig gradual_recursive.


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
