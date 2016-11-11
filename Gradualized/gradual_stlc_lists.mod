module gradual_stlc_lists.


typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (E x) T2).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (zero) (int).
typeOf (tt) (bool).
typeOf (ff) (bool).
typeOf (emptyList T) (list T).
typeOf (isnil T E) (bool) :- typeOf E (list T).
typeOf (cons T E1 E2) (list T) :- typeOf E1 T, typeOf E2 (list T).
typeOf (head T E) T :- typeOf E (list T).
typeOf (tail T E) (list T) :- typeOf E (list T).

value (abs T E).
value (zero).
value (tt).
value (ff).
value (emptyList T).
value (cons T V1 V2) :- value V1, value V2.

step (app (abs T E1) E2) (E1 E2) :- value E2.
step (isnil T (emptyList T)) (tt).
step (isnil T (cons T V1 V2)) (ff) :- value V1, value V2.
step (head T (cons T V1 V2)) V1 :- value V1, value V2.
step (tail T (cons T V1 V2)) V2 :- value V1, value V2.

multistep E E.
multistep E1 E3 :- step E1 E2, multistep E2 E3. 

% context cons 2[], 3[2].
% context head 2[].
% context tail 2[].
% context isnil 2[].
% eliminator app 1.
% eliminator head 2.
% eliminator tail 2.
% eliminator isnil 2.


step (cons E1 E2 E3) (cons E1 E2' E3) :- step E2 E2'.
step (cons E1 E2 E3) (cons E1 E2 E3') :- step E3 E3', value E2.
step (head E1 E2) (head E1 E2') :- step E2 E2'.
step (tail E1 E2) (tail E1 E2') :- step E2 E2'.
step (isnil E1 E2) (isnil E1 E2') :- step E2 E2'.
contains E E.
contains (cons E1 E2 E3) E :- contains E2 E.
contains (cons E1 E2 E3) E :- contains E3 E, value E2.
contains (head E1 E2) E :- contains E2 E.
contains (tail E1 E2) E :- contains E2 E.
contains (isnil E1 E2) E :- contains E2 E.

consistency X1 X2 :- join2 X1 X2 JoinX.
join2 (dyn) X X.
join2 X (dyn) X.
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (bool) (bool) (bool).
join2 (list X1) (list Y1) (list Z1) :- join2 X1 Y1 Z1.
join2 (int) (int) (int).
join2 (dyn) (dyn) (dyn).
ground (arrow (dyn) (dyn)).
ground (bool).
ground (list (dyn)).
ground (int).
value (cast V G L (dyn)) :- value V, ground G.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
value (cast V (list T1) L (list T1')) :- value V.
typeOfCC (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (E x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (zero) (int).
typeOfCC (tt) (bool).
typeOfCC (ff) (bool).
typeOfCC (emptyList T) (list T).
typeOfCC (isnil T E) (bool) :- typeOfCC E (list T).
typeOfCC (cons T E1 E2) (list T) :- typeOfCC E1 T, typeOfCC E2 (list T).
typeOfCC (head T E) T :- typeOfCC E (list T).
typeOfCC (tail T E) (list T) :- typeOfCC E (list T).
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1, consistency T1 T2.
typeOfCC (blame T L) T.
getGroundOf (arrow X1 X2) (arrow (dyn) (dyn)).
getGroundOf (bool) (bool).
getGroundOf (list X1) (list (dyn)).
getGroundOf (int) (int).
sameGround T1 T2 :- getGroundOf T1 X, getGroundOf T2 X.
contains (cast E1 T1 L T2) E :- contains E1 E.
containsError (cast E1 T1 L T2) E :- containsError E1 E.
step (cast E T1 L T2) (cast E' T1 L T2) :- step E E'.
stepC (cast V T L T) V :- value V.
stepC (cast (cast V G L1 (dyn)) (dyn) L2 G) V :- value V, ground G.
stepC (cast (cast V G1 L1 (dyn)) (dyn) L2 G2) (blame G2 L2) :- value V, ground G1, ground G2, not (sameGround G1 G2).
stepC (cast V T L (dyn)) (cast (cast V T L G) G L (dyn)) :- value V, getGroundOf T G, not (ground T).
stepC (cast V (dyn) L T) (cast (cast V (dyn) L G) G L T) :- value V, getGroundOf T G, not (ground T).
stepC E (blame T1 L) :- typeOfCC E T1, contains E (blame T2 L), not (E is (blame T2 L)).
stepC (app (cast V (arrow T1' T2') L (arrow T1 T2)) E2) (cast (app V (cast E2 T1 L T1')) T2' L T2) :- value V, value E2.
stepC (head T (cast V (list T') L (list T))) (cast (head T' V) T' L T) :- value V.
stepC (tail T (cast V (list T') L (list T))) (cast (tail T' V) (list T') L (list T)) :- value V.
stepC (isnil T (cast V (list T') L (list T))) (isnil T' V) :- value V.
step E E' :- stepC E E'.
