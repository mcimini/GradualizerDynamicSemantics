module recursive.

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (R x) T2).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (zero) (int).
typeOf (fold E R) (mu R) :- typeOf E (R (mu R)).
typeOf (unfold E) (R (mu R)) :- typeOf E (mu R).

value (abs T E).
value (zero).
value (fold V R) :- value V.

step (app (abs T E1) E2) (E1 E2) :- value E2.
step (unfold (fold V R)) V :- value V.

multistep E E.
multistep E1 E3 :- step E1 E2, multistep E2 E3. 

% context app 1[], 2[1].
% context fold 1[].
% context unfold 1[].
% eliminator app 1.
% eliminator unfold 1.
