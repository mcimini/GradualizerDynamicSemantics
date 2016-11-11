module stlc_lists.

typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (E x) T2).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (zero) (int).
typeOf (tt) (bool).
typeOf (ff) (bool).
typeOf (emptyList T) (list T).
typeOf (isnil T E) (bool) :- typeOf E (list T).
typeOf (cons T E1 E2) (list T) :- typeOf E1 T, typeOf E2 (list T).
typeOf (head T E) T :- typeOf E (list T).
typeOf (tail T E) (list T) :- typeOf E (list T).

value (abs T E).
value (zero).
value (tt).
value (ff).
value (emptyList T).
value (cons T V1 V2) :- value V1, value V2.

step (app (abs T E1) E2) (E1 E2) :- value E2.
step (isnil T (emptyList T)) (tt).
step (isnil T (cons T V1 V2)) (ff) :- value V1, value V2.
step (head T (cons T V1 V2)) V1 :- value V1, value V2.
step (tail T (cons T V1 V2)) V2 :- value V1, value V2.

multistep E E.
multistep E1 E3 :- step E1 E2, multistep E2 E3. 

% context cons 2[], 3[2].
% context head 2[].
% context tail 2[].
% context isnil 2[].
% eliminator app 1.
% eliminator head 2.
% eliminator tail 2.
% eliminator isnil 2.
