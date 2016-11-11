module stlc_exc.

typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (E x) T2).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (zero) (int).
typeOf (raise T E) T :- typeOf E (excType).
typeOf (try E1 E2) T :- typeOf E1 T, typeOf E2 (arrow (excType) T).

value (abs T E).
value (zero).
error (raise T V) :- value V.

step (app (abs T E1) E2) (E1 E2) :- value E2.
step (try V E)  V :- value V.
step (try (raise T V) E)  (app E V) :- value V.

step E (raise T1 V) :- typeOfCC V T1, containsError E (raise T2 V), value V.

multistep E E.
multistep E1 E3 :- step E1 E2, multistep E2 E3. 

% context app 1[], 2[1].
% context try 1[].
% context raise 2[].
% eliminator app 1.
% errorHandler try.
