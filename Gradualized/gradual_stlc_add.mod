module gradual_stlc_add.


typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (E x) T2).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (add E1 E2) (int) :- typeOf E1 (int), typeOf E2 (int).
typeOf (zero) (int).
typeOf (succ E) (int) :- typeOf E (int).

value (abs T E).
value (succ E) :- value E.
value (zero).

typeOf (zero) (int).

multistep E E.
multistep E1 E3 :- step E1 E2, multistep E2 E3. 


step (app E1 E2) (app E1' E2) :- step E1 E1'.
step (app E1 E2) (app E1 E2') :- step E2 E2', value E1.
step (add E1 E2) (add E1' E2) :- step E1 E1'.
step (add E1 E2) (add E1 E2') :- step E2 E2', value E1.
step (succ E1) (succ E1') :- step E1 E1'.
contains E E.
contains (app E1 E2) E :- contains E1 E.
contains (app E1 E2) E :- contains E2 E, value E1.
contains (add E1 E2) E :- contains E1 E.
contains (add E1 E2) E :- contains E2 E, value E1.
contains (succ E1) E :- contains E1 E.

consistency X1 X2 :- join2 X1 X2 JoinX.
join2 (dyn) X X.
join2 X (dyn) X.
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (int) (int) (int).
join2 (dyn) (dyn) (dyn).
ground (arrow (dyn) (dyn)).
ground (int).
value (cast V G L (dyn)) :- value V, ground G.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
typeOfCC (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (E x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (add E1 E2) (int) :- typeOfCC E1 (int), typeOfCC E2 (int).
typeOfCC (zero) (int).
typeOfCC (succ E) (int) :- typeOfCC E (int).
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1, consistency T1 T2.
typeOfCC (blame T L) T.
contains (cast E1 T1 L T2) E :- contains E1 E.
stepC (cast V T L T) V :- value V.
stepC (cast (cast V G L1 (dyn)) (dyn) L2 G) V :- value V, ground G.
stepC (cast (cast V G1 L1 (dyn)) (dyn) L2 G2) (blame G2 L2) :- value V, ground G1, ground G2, not (sameGround G1 G2).
stepC (cast V T L (dyn)) (cast (cast V T L G) G L (dyn)) :- value V, getGroundOf T G, not (ground T).
stepC (cast V (dyn) L T) (cast (cast V (dyn) L G) G L T) :- value V, getGroundOf T G, not (ground T).
getGroundOf (arrow X1 X2) (arrow (dyn) (dyn)).
getGroundOf (int) (int).
sameGround T1 T2 :- getGroundOf T1 X, getGroundOf T2 X.
step (cast E T1 L T2) (cast E' T1 L T2) :- step E E'.
step E E' :- stepC E E'.
step E (blame T1 L) :- typeOfCC E T1, contains E (blame T2 L).