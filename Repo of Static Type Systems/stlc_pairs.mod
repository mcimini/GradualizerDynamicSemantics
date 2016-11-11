module stlc_pairs.

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
