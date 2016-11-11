module stlc_ref.

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
