module gradual_stlc_ref.


typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (E x) T2).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (zero) (int).
typeOf (unit) (unitType).
typeOf (ref E) (refType T) :- typeOf E T.
typeOf (deref E) T :- typeOf E (refType T).
typeOf (assign E1 E2) (unitType) :- typeOf E1 (refType T), typeOf E2 T.


value (abs T E).
value (zero).
value (unit).
value (loc Addr).

stepRef (app (abs T E1) E2) Mu (E1 E2) Mu :- value E2.
stepRef (ref V) Mu (Loc Addr) (heapCons Addr V Mu) :- fresh Addr Mu, value V.
stepRef (assign (loc Addr) V) Mu (unit) (heapCons Addr V Mu) :- value V.
stepRef (deref (loc Addr)) Mu V Mu :- heapLookup Mu Addr V.

stepRef E Mu E' Mu :- step E E'.

heapLookup (heapCons Addr E Mu) Addr E.
heapLookup (heapCons Addr1 E1 Mu) Addr2 E2 :- heapLookup Mu Addr2 E2.

% The predicate fresh generates a new address not in the heap Mu. 
% This predicate is left unspecified. Lambda-prolog, just like functional programs, is pure.
% stlc_ref simply demonstrates that the Gradualizer can generate the correct rules for assign and deref in gradual_stlc_ref.

multistepRef E Mu E Mu.
multistepRef E1 Mu1 E3 Mu3 :- stepRef E1 Mu1 E2 Mu2, multistepRef E2 Mu2 E3 Mu3. 

% context app 1[], 2[1].
% context ref 1[].
% context deref 1[].
% context assign 1[],2[1].
% eliminator app 1.
% eliminator assign 1.
% eliminator deref 1.


step (app E1 E2) (app E1' E2) :- step E1 E1'.
step (app E1 E2) (app E1 E2') :- step E2 E2', value E1.
step (ref E1) (ref E1') :- step E1 E1'.
step (deref E1) (deref E1') :- step E1 E1'.
step (assign E1 E2) (assign E1' E2) :- step E1 E1'.
step (assign E1 E2) (assign E1 E2') :- step E2 E2', value E1.
contains E E.
contains (app E1 E2) E :- contains E1 E.
contains (app E1 E2) E :- contains E2 E, value E1.
contains (ref E1) E :- contains E1 E.
contains (deref E1) E :- contains E1 E.
contains (assign E1 E2) E :- contains E1 E.
contains (assign E1 E2) E :- contains E2 E, value E1.

consistency X1 X2 :- join2 X1 X2 JoinX.
join2 (dyn) X X.
join2 X (dyn) X.
join2 (int) (int) (int).
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (refType X1) (refType Y1) (refType Z1) :- join2 X1 Y1 Z1.
join2 (unitType) (unitType) (unitType).
join2 (dyn) (dyn) (dyn).
ground (int).
ground (arrow (dyn) (dyn)).
ground (refType (dyn)).
ground (unitType).
value (cast V G L (dyn)) :- value V, ground G.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
value (cast V (refType T1) L (refType T1')) :- value V.
typeOfCC (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (E x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (zero) (int).
typeOfCC (unit) (unitType).
typeOfCC (ref E) (refType T) :- typeOfCC E T.
typeOfCC (deref E) T :- typeOfCC E (refType T).
typeOfCC (assign E1 E2) (unitType) :- typeOfCC E1 (refType T), typeOfCC E2 T.
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1, consistency T1 T2.
typeOfCC (blame T L) T.
getGroundOf (int) (int).
getGroundOf (arrow X1 X2) (arrow (dyn) (dyn)).
getGroundOf (refType X1) (refType (dyn)).
getGroundOf (unitType) (unitType).
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
stepC (app (cast V (arrow T1' T2') L (arrow T1 T2)) E2) (cast (app V (cast E2 T1 L T1')) T2' L T2) :- value V.
stepC (assign (cast V (refType T') L (refType T)) E2) (assign V (cast E2 T L T')) :- value V.
stepC (deref (cast V (refType T') L (refType T))) (cast (deref V) T' L T) :- value V.
step E E' :- stepC E E'.
