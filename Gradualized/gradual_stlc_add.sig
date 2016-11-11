sig gradual_stlc_add.


kind	term			type.
kind	typ				type.

type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.

type	add		term -> term -> term.

type	int			typ.
type	zero		term.
type	succ		term -> term.
type	multistep			term -> term -> o.


% operatorInfo abs :- constructor, arrow.
% operatorInfo app :- deconstructor, arrow, 1, contravariant, 1.
% operatorInfo zero :- constructor, int.
% operatorInfo succ :- constructor, int.
% operatorInfo add :- deconstructor, int, 1, covariant, 1.
% context app 1[], 2[1].
% context add 1[],2[1].
% context succ 1[].
% mode typeOf inp -> out.
% mode step inp -> out.


kind label	 type.

type dyn		typ.
type consistency		typ -> typ -> o.
type join2		typ -> typ -> typ -> o.
type contains		term -> term -> o.
type ground		typ -> o.
type cast		term -> typ -> label -> typ -> term.
type blame		typ -> label -> term.
type typeOfCC		term -> typ -> o.
type stepC		term -> term -> o.
type getGroundOf		typ -> typ -> o.
type sameGround		typ -> typ -> o.
type compToCC		term -> term -> typ -> o.
