module stlc_tuples.

typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (E x) T2).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (zero) (int).
typeOf (tuple E1 E2 E3 E4) (tupleType T1 T2 T3 T4) :- typeOf E1 T1, typeOf E2 T2, typeOf E3 T3, typeOf E4 T4.
typeOf (select1 E) T1 :- typeOf E (tupleType T1 T2 T3 T4).
typeOf (select2 E) T2 :- typeOf E (tupleType T1 T2 T3 T4).
typeOf (select3 E) T3 :- typeOf E (tupleType T1 T2 T3 T4).
typeOf (select4 E) T4 :- typeOf E (tupleType T1 T2 T3 T4).

value (abs T E).
value (zero).
value (tuple V1 V2 V3 V4) :- value V1, value V2, value V3, value V4. 

step (app (abs T E1) E2) (E1 E2) :- value E2.
step (select1 (tuple V1 V2 V3 V4)) E1 :- value V1, value V2, value V3, value V4. 
step (select2 (tuple V1 V2 V3 V4)) E2 :- value V1, value V2, value V3, value V4. 
step (select3 (tuple V1 V2 V3 V4)) E3 :- value V1, value V2, value V3, value V4. 
step (select4 (tuple V1 V2 V3 V4)) E4 :- value V1, value V2, value V3, value V4. 


multistep E E.
multistep E1 E3 :- step E1 E2, multistep E2 E3. 

% context app 1[], 2[1].
% context tuple 1[],2[1],3[1,2],4[1,2,3].
% context select1 1[].
% context select2 1[].
% context select3 1[].
% context select4 1[].
% eliminator app 1.
% eliminator select1 1.
% eliminator select2 1.
% eliminator select3 1.
% eliminator select4 1.
