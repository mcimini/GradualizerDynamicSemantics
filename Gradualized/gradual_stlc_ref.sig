sig gradual_stlc_ref.


kind	term			type.
kind	typ				type.

type	int			typ.
type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.
type	zero		term.

type	typeOf			term -> typ -> o. 
type	step			term -> term -> o.
type	stepRef			term -> heap -> term -> heap -> o.
type	value			term -> o.

type	refType		typ -> typ.
type	unitType	typ.

type	unit		term.
type	ref			term -> term.
type	deref		term -> term.
type	assign		term -> term -> term.

% addresses and environment
kind 	address			type.
type	loc				address -> term. 

% heaps
kind 	heap				type.

type 		emptyHeap	heap.
type 		heapCons	address -> term -> heap -> heap.
type		heapLookup	heap -> address -> term -> o.

type		fresh		address -> heap -> o.

type	multistepRef	term -> heap -> term -> heap -> o.




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
