module gradual_stlc_tuples.


typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (E x) T2).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (zero) (int).
typeOf (tuple E1 E2 E3 E4) (tupleType T1 T2 T3 T4) :- typeOf E1 T1, typeOf E2 T2, typeOf E3 T3, typeOf E4 T4.
typeOf (select1 E) T1 :- typeOf E (tupleType T1 T2 T3 T4).
typeOf (select2 E) T2 :- typeOf E (tupleType T1 T2 T3 T4).
typeOf (select3 E) T3 :- typeOf E (tupleType T1 T2 T3 T4).
typeOf (select4 E) T4 :- typeOf E (tupleType T1 T2 T3 T4).

value (abs T E).
value (zero).
value (tuple V1 V2 V3 V4) :- value V1, value V2, value V3, value V4. 

step (app (abs T E1) E2) (E1 E2) :- value E2.
step (select1 (tuple V1 V2 V3 V4)) E1 :- value V1, value V2, value V3, value V4. 
step (select2 (tuple V1 V2 V3 V4)) E2 :- value V1, value V2, value V3, value V4. 
step (select3 (tuple V1 V2 V3 V4)) E3 :- value V1, value V2, value V3, value V4. 
step (select4 (tuple V1 V2 V3 V4)) E4 :- value V1, value V2, value V3, value V4. 


multistep E E.
multistep E1 E3 :- step E1 E2, multistep E2 E3. 

% context app 1[], 2[1].
% context tuple 1[],2[1],3[1,2],4[1,2,3].
% context select1 1[].
% context select2 1[].
% context select3 1[].
% context select4 1[].
% eliminator app 1.
% eliminator select1 1.
% eliminator select2 1.
% eliminator select3 1.
% eliminator select4 1.


step (app E1 E2) (app E1' E2) :- step E1 E1'.
step (app E1 E2) (app E1 E2') :- step E2 E2', value E1.
step (tuple E1 E2 E3 E4) (tuple E1' E2 E3 E4) :- step E1 E1'.
step (tuple E1 E2 E3 E4) (tuple E1 E2' E3 E4) :- step E2 E2', value E1.
step (tuple E1 E2 E3 E4) (tuple E1 E2 E3' E4) :- step E3 E3', value E1, value E2.
step (tuple E1 E2 E3 E4) (tuple E1 E2 E3 E4') :- step E4 E4', value E1, value E2, value E3.
step (select1 E1) (select1 E1') :- step E1 E1'.
step (select2 E1) (select2 E1') :- step E1 E1'.
step (select3 E1) (select3 E1') :- step E1 E1'.
step (select4 E1) (select4 E1') :- step E1 E1'.
contains E E.
contains (app E1 E2) E :- contains E1 E.
contains (app E1 E2) E :- contains E2 E, value E1.
contains (tuple E1 E2 E3 E4) E :- contains E1 E.
contains (tuple E1 E2 E3 E4) E :- contains E2 E, value E1.
contains (tuple E1 E2 E3 E4) E :- contains E3 E, value E1, value E2.
contains (tuple E1 E2 E3 E4) E :- contains E4 E, value E1, value E2, value E3.
contains (select1 E1) E :- contains E1 E.
contains (select2 E1) E :- contains E1 E.
contains (select3 E1) E :- contains E1 E.
contains (select4 E1) E :- contains E1 E.

consistency X1 X2 :- join2 X1 X2 JoinX.
join2 (dyn) X X.
join2 X (dyn) X.
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (tupleType X1 X2 X3 X4) (tupleType Y1 Y2 Y3 Y4) (tupleType Z1 Z2 Z3 Z4) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2, join2 X3 Y3 Z3, join2 X4 Y4 Z4.
join2 (int) (int) (int).
join2 (dyn) (dyn) (dyn).
ground (arrow (dyn) (dyn)).
ground (tupleType (dyn) (dyn) (dyn) (dyn)).
ground (int).
value (cast V G L (dyn)) :- value V, ground G.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
value (cast V (tupleType T1 T2 T3 T4) L (tupleType T1' T2' T3' T4')) :- value V.
typeOfCC (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (E x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (zero) (int).
typeOfCC (tuple E1 E2 E3 E4) (tupleType T1 T2 T3 T4) :- typeOfCC E1 T1, typeOfCC E2 T2, typeOfCC E3 T3, typeOfCC E4 T4.
typeOfCC (select1 E) T1 :- typeOfCC E (tupleType T1 T2 T3 T4).
typeOfCC (select2 E) T2 :- typeOfCC E (tupleType T1 T2 T3 T4).
typeOfCC (select3 E) T3 :- typeOfCC E (tupleType T1 T2 T3 T4).
typeOfCC (select4 E) T4 :- typeOfCC E (tupleType T1 T2 T3 T4).
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1, consistency T1 T2.
typeOfCC (blame T L) T.
getGroundOf (arrow X1 X2) (arrow (dyn) (dyn)).
getGroundOf (tupleType X1 X2 X3 X4) (tupleType (dyn) (dyn) (dyn) (dyn)).
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
stepC (select1 (cast V (tupleType T1' T2' T3' T4') L (tupleType T1 T2 T3 T4))) (cast (select1 V) T1' L T1) :- value V.
stepC (select2 (cast V (tupleType T1' T2' T3' T4') L (tupleType T1 T2 T3 T4))) (cast (select2 V) T2' L T2) :- value V.
stepC (select3 (cast V (tupleType T1' T2' T3' T4') L (tupleType T1 T2 T3 T4))) (cast (select3 V) T3' L T3) :- value V.
stepC (select4 (cast V (tupleType T1' T2' T3' T4') L (tupleType T1 T2 T3 T4))) (cast (select4 V) T4' L T4) :- value V.
step E E' :- stepC E E'.
