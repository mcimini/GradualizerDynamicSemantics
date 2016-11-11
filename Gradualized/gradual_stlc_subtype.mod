module gradual_stlc_subtype.


typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (E x) T2).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T3, subtype T1 T3.

value (abs T E).
value (zero).

step (app (abs T E1) E2) (E1 E2).

typeOf (zero) (int).

multistep E E.
multistep E1 E3 :- step E1 E2, multistep E2 E3. 


contains E E.
step (app E1 E2) (app E1' E2) :- step E1 E1'.
contains (app E1 E2) E :- contains E1 E.
step (app E1 E2) (app E1 E2') :- step E2 E2', value E1.
contains (app E1 E2) E :- contains E2 E, value E1.

typeOfGr (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfGr x T1 => typeOfGr (E x) T2)), typeOfGr (E x) T2.
typeOfGr (app E1 E2) T2 :- typeOfGr E1 PM1, matchArrow PM1 T1 T2, typeOfGr E2 T3, subtypeG T1 T3.
typeOfGr (zero) (int).
matchInt (int).
matchInt (dyn).
matchArrow (arrow T1 T2) T1 T2.
matchArrow (dyn) (dyn) (dyn).
matchDyn (dyn).
join2 (int) (int) (int).
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (dyn) (dyn) (dyn).
subtypeG X1 X3 :- flow X1 X2, subtype X2 X3.
subtypeCI X1 X3 X2 :- flow X1 X2, subtype X2 X3.
ground (int).
ground (arrow (dyn) (dyn)).
value (cast V G L (dyn)) :- value V, ground G.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
typeOfCC (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (E x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T3, subtype T1 T3.
typeOfCC (zero) (int).
step (cast E T1 L T2) (cast E' T1 L T2) :- step E E'.
contains (cast E1 T1 L T2) E :- contains E1 E.
step (cast V T L T) V :- value V.
step (cast E (dyn) L2 G) V :- E = (cast V G L1 (dyn)), value V, ground G.
step (cast E (dyn) L2 G2) (blame L2) :- E = (cast V G1 L1 (dyn)), value V, ground G1, ground G2, not (sameGround G1 G2).
step (cast V A L (dyn)) (cast (cast V A L G) G L (dyn)) :- value V, getGroundOf A G, not (ground A).
step (cast V (dyn) L A) (cast (cast V (dyn) L G) G L A) :- value V, getGroundOf A G, not (ground A).
step E (blame L) :- contains E (blame L), not (E = (blame L)).
getGroundOf (int) (int).
getGroundOf (arrow X1 X2) (arrow (dyn) (dyn)).
sameGround T1 T2 :- getGroundOf T1 X, getGroundOf T2 X.
compToCC (abs T1 E) (abs T1 (x\ (E' x))) (arrow T1 T2) :- (pi x\ (compToCC x x T1 => compToCC (E x) (E' x) T2)), compToCC (E x) (E' x) T2.
compToCC (app E1 E2) (app (cast E1' PM1 L (arrow New1 T2)) E2') T2 :- compToCC E1 E1' PM1, matchArrow PM1 T1 T2, compToCC E2 E2' T3, subtypeCI T1 T3 New1, flow T1 New1.
compToCC (zero) (zero) (int).
