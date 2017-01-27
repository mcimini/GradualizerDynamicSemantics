module gradual_stlc_sum.


typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (E x) T2).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (zero) (int).
typeOf (case E E1 E2) T :- typeOf E (sum T1 T2), (pi x\ typeOf x T1 => typeOf (E1 x) T), (pi x\ typeOf x T2 => typeOf (E2 x) T).
typeOf (inl T2 E) (sum T1 T2) :- typeOf E T1.
typeOf (inr T1 E) (sum T1 T2) :- typeOf E T2.

value (abs T E).
value (zero).
value (inl T V) :- value V.
value (inr T V) :- value V.

step (app (abs T E1) E2) (E1 E2) :- value E2.
step (case (inl T V) E1 E2) (E1 V) :- value V.
step (case (inr T V) E1 E2) (E2 V) :- value V.


multistep E E.
multistep E1 E3 :- step E1 E2, multistep E2 E3. 

% context app 1[], 2[1].
% context inr 2[].
% context inl 2[].
% context case 1[].
% eliminator app 1.
% eliminator case 1.


step (app E1 E2) (app E1' E2) :- step E1 E1'.
step (app E1 E2) (app E1 E2') :- step E2 E2', value E1.
step (inr E1 E2) (inr E1 E2') :- step E2 E2'.
step (inl E1 E2) (inl E1 E2') :- step E2 E2'.
step (case E1 E2 E3) (case E1' E2 E3) :- step E1 E1'.
contains E E.
contains (app E1 E2) E :- contains E1 E.
contains (app E1 E2) E :- contains E2 E, value E1.
contains (inr E1 E2) E :- contains E2 E.
contains (inl E1 E2) E :- contains E2 E.
contains (case E1 E2 E3) E :- contains E1 E.

consistency X1 X2 :- join2 X1 X2 JoinX.
join2 (dyn) X X.
join2 X (dyn) X.
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (sum X1 X2) (sum Y1 Y2) (sum Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (int) (int) (int).
join2 (dyn) (dyn) (dyn).
ground (arrow (dyn) (dyn)).
ground (sum (dyn) (dyn)).
ground (int).
value (cast V G L (dyn)) :- value V, ground G.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
value (cast V (sum T1 T2) L (sum T1' T2')) :- value V.
typeOfCC (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (E x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (zero) (int).
typeOfCC (case E E1 E2) T :- typeOfCC E (sum T1 T2), (pi x\ (typeOfCC x T1 => typeOfCC (E1 x) T)), (pi x\ (typeOfCC x T2 => typeOfCC (E2 x) T)).
typeOfCC (inl T2 E) (sum T1 T2) :- typeOfCC E T1.
typeOfCC (inr T1 E) (sum T1 T2) :- typeOfCC E T2.
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1, consistency T1 T2.
typeOfCC (blame T L) T.
getGroundOf (arrow X1 X2) (arrow (dyn) (dyn)).
getGroundOf (sum X1 X2) (sum (dyn) (dyn)).
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
stepC (app (cast V (arrow T1' T2') L (arrow T1 T2)) V2) (cast (app V (cast V2 T1 L T1')) T2' L T2) :- value V, value V2.
stepC (case (cast V (sum T1' T2') L (sum T1 T2)) E1 E2) (case V (x\ (E1 (cast x T1' L T1))) (x\ (E2 (cast x T2' L T2)))) :- value V.
step E E' :- stepC E E'.
