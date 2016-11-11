module fpl.

typeOf (fold E R) (mu R) :- typeOf E (R (mu R)).

typeOf (absT R2) (all R) :- (pi x\ typeOf (R2 x) (R x)).

typeOf (cons T E1 E2) (list T) :- typeOf E1 T, typeOf E2 (list T).

typeOf (emptyList T) (list T).

typeOf (inr T1 E) (sum T1 T2) :- typeOf E T2.

typeOf (inl T2 E) (sum T1 T2) :- typeOf E T1.

typeOf (pair E1 E2) (times T1 T2) :- typeOf E1 T1, typeOf E2 T2.

typeOf (ff ) (bool ).

typeOf (tt ) (bool ).

typeOf (succ E) (int ) :- typeOf E (int ).

typeOf (zero ) (int ).

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (R x) T2).

typeOf (unfold E) (R (mu R)) :- typeOf E (mu R).

step (unfold (fold V R)) V :- value V.

typeOf (appT E T) (R T) :- typeOf E (all R).

step (appT (absT R2) T) (R2 T).

typeOf (isnil T E) (bool ) :- typeOf E (list T).

step (isnil T (emptyList T')) (tt ).

step (isnil T (cons T V1 V2)) (ff) :- value V1, value V2.

typeOf (tail T E) (list T) :- typeOf E (list T).

step (tail T (emptyList T')) (raise (list T) (succ (zero ))).

step (tail T (cons T V1 V2)) V2 :- value V1, value V2.

typeOf (head T E) T :- typeOf E (list T).

step (head T (emptyList T')) (raise T (zero )).

step (head T (cons T V1 V2)) V1 :- value V1, value V2.

typeOf (case EE R1 R2) T :- typeOf EE (sum T1 T2), (pi x\ typeOf x T1 => typeOf (R1 x) T), (pi x\ typeOf x T2 => typeOf (R2 x) T).

step (case (inl T V) E1 E2) (E1 V) :- value V.

step (case (inr T V) E1 E2) (E2 V) :- value V.

typeOf (snd E) T2 :- typeOf E (times T1 T2).

step (snd (pair V1 V2)) V2 :- value V1, value V2.

typeOf (fst E) T1 :- typeOf E (times T1 T2).

step (fst (pair V1 V2)) V1 :- value V1, value V2.

typeOf (if E1 E2 E3) T :- typeOf E1 (bool ), typeOf E2 T, typeOf E3 T.

step (if (tt ) E1 E2) E1.

step (if (ff ) E1 E2) E2.

typeOf (isZero E) (bool ) :- typeOf E (int ).

step (isZero (zero )) (tt ).

step (isZero (succ V)) (ff ) :- value V.

typeOf (pred E) (int ) :- typeOf E (int ).

step (pred (zero )) (raise (int) (zero )).

step (pred (succ V)) V :- value V.

typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.

step (app (abs T R) V) (R V) :- value V.

typeOf (let E R) T2 :- typeOf E T1, (pi x\ typeOf x T1 => typeOf (R x) T2).

step (let V R) (R V) :- value V.

typeOf (letrec T1 R1 R2) T2 :- (pi x\ typeOf x T1 => typeOf (R1 x) T1), (pi x\ typeOf x T1 => typeOf (R2 x) T2).

step (letrec T1 R1 R2) (R2 (fix (abs T1 R1))).

typeOf (fix E) T :- typeOf E (arrow T T).

step (fix V) (app V (fix V)) :- value V.

typeOf (try E1 E2) T :- typeOf E1 T, typeOf E2 (arrow (int ) T).

step (try E1 E2) E1 :- value E1.

step (try (raise T V) E)  (app E V) :- value V.

value (fold E1 U2) :- value E1.

value (absT R1).

value (cons T1 E2 E3) :- value E2, value E3.

value (emptyList T1).

value (inr T V) :- value V.

value (inl T E1) :- value E1.

value (pair E1 E2) :- value E1, value E2.

value (ff ).

value (tt ).

value (succ E1) :- value E1.

value (zero ).

value (abs T1 R2).

error (raise T V) :- value V. 

typeOf (raise T E) T :- typeOf E (int ).

step E (raise T1 V) :- typeOfCC E T1, containsError E (raise T2 V), value V. 

multistep E E.

multistep E1 E3 :- step E1 E2, multistep E2 E3.

% context app 1[], 2[1].
% context if 1[].
% context succ 1[].
% context pred 1[].
% context isZero 1[].
% context pair 1[], 2[1].
% context fst 1[].
% context snd 1[].
% context inr 2[].
% context inl 2[].
% context case 1[].
% context cons 2[], 3[2].
% context head 2[].
% context tail 2[].
% context isnil 2[].
% context appT 1[].
% context fold 1[].
% context unfold 1[].
% context let 1[].
% context fix 1[].
% context try 1[].
% context raise 2[].
% eliminator app 1.
% eliminator if 1.
% eliminator pred 1.
% eliminator isZero 1.
% eliminator fst 1.
% eliminator snd 1.
% eliminator case 1.
% eliminator head 2.
% eliminator tail 2.
% eliminator isnil 2.
% eliminator appT 1.
% eliminator unfold 1.
% errorHandler try.
