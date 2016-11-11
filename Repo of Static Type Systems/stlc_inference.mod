module stlc_inference.

typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (absInf E) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (E x) T2).
typeOf (zero) (int).

value (absInf E).
value (zero).

step (app (absInf E1) E2) (E1 E2) :- value E2.

multistep E E.
multistep E1 E3 :- step E1 E2, multistep E2 E3. 

% context app 1[], 2[1].
% eliminator app 1.
