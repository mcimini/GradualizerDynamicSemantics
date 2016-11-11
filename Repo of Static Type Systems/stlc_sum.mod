module stlc_sum.

typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (E x) T2).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (zero) (int).
typeOf (case E E1 E2) T :- typeOf E (sum T1 T2), (pi x\ typeOf x T1 => typeOf (E1 x) T), (pi x\ typeOf x T2 => typeOf (E2 x) T).
typeOf (inl T2 E) (sum T1 T2) :- typeOf E T1.
typeOf (inr T1 E) (sum T1 T2) :- typeOf E T2.

value (abs T E).
value (zero).
value (inl T V) :- value V.
value (inr T V) :- value V.

step (app (abs T E1) E2) (E1 E2) :- value E2.
step (case (inl T V) E1 E2) (E1 V) :- value V.
step (case (inr T V) E1 E2) (E2 V) :- value V.


multistep E E.
multistep E1 E3 :- step E1 E2, multistep E2 E3. 

% context app 1[], 2[1].
% context inr 2[].
% context inl 2[].
% context case 1[].
% eliminator app 1.
% eliminator case 1.
