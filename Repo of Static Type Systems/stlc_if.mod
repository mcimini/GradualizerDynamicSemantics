module stlc_if.

typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (E x) T2).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (zero) (int).
typeOf (if E1 E2 E3) T :- typeOf E1 (bool), typeOf E2 T, typeOf E3 T.
typeOf (tt) (bool).
typeOf (ff) (bool).

value (abs T E).
value (tt).
value (ff).
value (zero).

step (app (abs T E1) E2) (E1 E2) :- value E2.
step (if (tt) E1 E2) E1. 
step (if (ff) E1 E2) E2. 


multistep E E.
multistep E1 E3 :- step E1 E2, multistep E2 E3. 

% context app 1[], 2[1].
% context if 1[].
% eliminator app 1.
% eliminator if 1.
