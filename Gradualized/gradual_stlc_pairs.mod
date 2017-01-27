module gradual_stlc_pairs.


typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (E x) T2).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (zero) (int).
typeOf (pair E1 E2) (times T1 T2) :- typeOf E1 T1, typeOf E2 T2.
typeOf (fst E) T1 :- typeOf E (times T1 T2).
typeOf (snd E) T2 :- typeOf E (times T1 T2).

value (abs T E).
value (zero).
value (pair V1 V2) :- value V1, value V2.

step (app (abs T E1) E2) (E1 E2) :- value E2.
step (fst (pair V1 V2)) V1 :- value V1, value V2.
step (snd (pair V1 V2)) V2 :- value V1, value V2.


multistep E E.
multistep E1 E3 :- step E1 E2, multistep E2 E3. 

% context app 1[], 2[1].
% context pair 1[],2[1].
% context fst 1[].
% context snd 1[].
% eliminator app 1.
% eliminator fst 1.
% eliminator snd 1.


step (app E1 E2) (app E1' E2) :- step E1 E1'.
step (app E1 E2) (app E1 E2') :- step E2 E2', value E1.
step (pair E1 E2) (pair E1' E2) :- step E1 E1'.
step (pair E1 E2) (pair E1 E2') :- step E2 E2', value E1.
step (fst E1) (fst E1') :- step E1 E1'.
step (snd E1) (snd E1') :- step E1 E1'.
contains E E.
contains (app E1 E2) E :- contains E1 E.
contains (app E1 E2) E :- contains E2 E, value E1.
contains (pair E1 E2) E :- contains E1 E.
contains (pair E1 E2) E :- contains E2 E, value E1.
contains (fst E1) E :- contains E1 E.
contains (snd E1) E :- contains E1 E.

consistency X1 X2 :- join2 X1 X2 JoinX.
join2 (dyn) X X.
join2 X (dyn) X.
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (times X1 X2) (times Y1 Y2) (times Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (int) (int) (int).
join2 (dyn) (dyn) (dyn).
ground (arrow (dyn) (dyn)).
ground (times (dyn) (dyn)).
ground (int).
value (cast V G L (dyn)) :- value V, ground G.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
value (cast V (times T1 T2) L (times T1' T2')) :- value V.
typeOfCC (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (E x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (zero) (int).
typeOfCC (pair E1 E2) (times T1 T2) :- typeOfCC E1 T1, typeOfCC E2 T2.
typeOfCC (fst E) T1 :- typeOfCC E (times T1 T2).
typeOfCC (snd E) T2 :- typeOfCC E (times T1 T2).
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1, consistency T1 T2.
typeOfCC (blame T L) T.
getGroundOf (arrow X1 X2) (arrow (dyn) (dyn)).
getGroundOf (times X1 X2) (times (dyn) (dyn)).
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
stepC (fst (cast V (times T1' T2') L (times T1 T2))) (cast (fst V) T1' L T1) :- value V.
stepC (snd (cast V (times T1' T2') L (times T1 T2))) (cast (snd V) T2' L T2) :- value V.
step E E' :- stepC E E'.
