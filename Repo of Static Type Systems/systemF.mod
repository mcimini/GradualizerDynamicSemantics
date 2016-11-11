module systemF.

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (R x) T2).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (zero) (int).
typeOf (absT R2) (all R) :- (pi x\ typeOf (R2 x) (R x)).
typeOf (appT E T) (R T) :- typeOf E (all R).

value (abs T E).
value (zero).
value (absT R).

step (app (abs T E1) E2) (E1 E2) :- value E2.
step (appT (absT R) T) (R T).

multistep E E.
multistep E1 E3 :- step E1 E2, multistep E2 E3. 

% context app 1[], 2[1].
% context appT 1[].
% eliminator app 1.
% eliminator appT 1.

