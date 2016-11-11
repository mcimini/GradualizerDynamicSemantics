module stlc_integers.

typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (E x) T2).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (zero) (int).
typeOf (succ E) (int ) :- typeOf E (int ).
typeOf (pred E) (int ) :- typeOf E (int ).

value (abs T E).
value (zero).
value (succ V) :- value V.

step (app (abs T E1) E2) (E1 E2) :- value E2.
step (pred (zero )) (error).
step (pred (succ V)) V :- value V.

multistep E E.
multistep E1 E3 :- step E1 E2, multistep E2 E3. 

% context app 1[], 2[1].
% context succ 1[].
% context pred 1[].
% eliminator app 1.
% eliminator pred 1.

