module gradual_recursive.


typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (R x) T2).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (zero) (int).
typeOf (fold E R) (mu R) :- typeOf E (R (mu R)).
typeOf (unfold E) (R (mu R)) :- typeOf E (mu R).

value (abs T E).
value (zero).
value (fold V R) :- value V.

step (app (abs T E1) E2) (E1 E2) :- value E2.
step (unfold (fold V R)) V :- value V.

multistep E E.
multistep E1 E3 :- step E1 E2, multistep E2 E3. 

% context app 1[], 2[1].
% context fold 1[].
% context unfold 1[].
% eliminator app 1.
% eliminator unfold 1.


step (app E1 E2) (app E1' E2) :- step E1 E1'.
step (app E1 E2) (app E1 E2') :- step E2 E2', value E1.
step (fold E1 E2) (fold E1' E2) :- step E1 E1'.
step (unfold E1) (unfold E1') :- step E1 E1'.
contains E E.
contains (app E1 E2) E :- contains E1 E.
contains (app E1 E2) E :- contains E2 E, value E1.
contains (fold E1 E2) E :- contains E1 E.
contains (unfold E1) E :- contains E1 E.

consistency X1 X2 :- join2 X1 X2 JoinX.
join2 (dyn) X X.
join2 X (dyn) X.
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (mu X1) (mu Y1) (mu Z1) :- (pi x\ join2 (X1 x) (Y1 x) (Z1 x)).
join2 (int) (int) (int).
join2 (dyn) (dyn) (dyn).
ground (arrow (dyn) (dyn)).
ground (mu (x\ (dyn))).
ground (int).
value (cast V G L (dyn)) :- value V, ground G.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
value (cast V (mu T1) L (mu T1')) :- value V.
typeOfCC (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (R x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (zero) (int).
typeOfCC (fold E R) (mu R) :- typeOfCC E (R (mu R)).
typeOfCC (unfold E) (R (mu R)) :- typeOfCC E (mu R).
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1, consistency T1 T2.
typeOfCC (blame T L) T.
getGroundOf (arrow X1 X2) (arrow (dyn) (dyn)).
getGroundOf (mu X1) (mu (x\ (dyn))).
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
stepC (unfold (cast V (mu R') L (mu R))) (cast (unfold V) (R' (mu R')) L (R (mu R))) :- value V.
step E E' :- stepC E E'.