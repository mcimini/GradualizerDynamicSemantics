module gradual_fpl.


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


step (app E1 E2) (app E1' E2) :- step E1 E1'.
step (app E1 E2) (app E1 E2') :- step E2 E2', value E1.
step (if E1 E2 E3) (if E1' E2 E3) :- step E1 E1'.
step (succ E1) (succ E1') :- step E1 E1'.
step (pred E1) (pred E1') :- step E1 E1'.
step (isZero E1) (isZero E1') :- step E1 E1'.
step (pair E1 E2) (pair E1' E2) :- step E1 E1'.
step (pair E1 E2) (pair E1 E2') :- step E2 E2', value E1.
step (fst E1) (fst E1') :- step E1 E1'.
step (snd E1) (snd E1') :- step E1 E1'.
step (inr E1 E2) (inr E1 E2') :- step E2 E2'.
step (inl E1 E2) (inl E1 E2') :- step E2 E2'.
step (case E1 E2 E3) (case E1' E2 E3) :- step E1 E1'.
step (cons E1 E2 E3) (cons E1 E2' E3) :- step E2 E2'.
step (cons E1 E2 E3) (cons E1 E2 E3') :- step E3 E3', value E2.
step (head E1 E2) (head E1 E2') :- step E2 E2'.
step (tail E1 E2) (tail E1 E2') :- step E2 E2'.
step (isnil E1 E2) (isnil E1 E2') :- step E2 E2'.
step (appT E1 E2) (appT E1' E2) :- step E1 E1'.
step (fold E1 E2) (fold E1' E2) :- step E1 E1'.
step (unfold E1) (unfold E1') :- step E1 E1'.
step (let E1 E2) (let E1' E2) :- step E1 E1'.
step (fix E1) (fix E1') :- step E1 E1'.
step (try E1 E2) (try E1' E2) :- step E1 E1'.
step (raise E1 E2) (raise E1 E2') :- step E2 E2'.
contains E E.
contains (app E1 E2) E :- contains E1 E.
contains (app E1 E2) E :- contains E2 E, value E1.
contains (if E1 E2 E3) E :- contains E1 E.
contains (succ E1) E :- contains E1 E.
contains (pred E1) E :- contains E1 E.
contains (isZero E1) E :- contains E1 E.
contains (pair E1 E2) E :- contains E1 E.
contains (pair E1 E2) E :- contains E2 E, value E1.
contains (fst E1) E :- contains E1 E.
contains (snd E1) E :- contains E1 E.
contains (inr E1 E2) E :- contains E2 E.
contains (inl E1 E2) E :- contains E2 E.
contains (case E1 E2 E3) E :- contains E1 E.
contains (cons E1 E2 E3) E :- contains E2 E.
contains (cons E1 E2 E3) E :- contains E3 E, value E2.
contains (head E1 E2) E :- contains E2 E.
contains (tail E1 E2) E :- contains E2 E.
contains (isnil E1 E2) E :- contains E2 E.
contains (appT E1 E2) E :- contains E1 E.
contains (fold E1 E2) E :- contains E1 E.
contains (unfold E1) E :- contains E1 E.
contains (let E1 E2) E :- contains E1 E.
contains (fix E1) E :- contains E1 E.
contains (try E1 E2) E :- contains E1 E.
contains (raise E1 E2) E :- contains E2 E.
containsError E E.
containsError (app E1 E2) E :- containsError E1 E.
containsError (app E1 E2) E :- containsError E2 E, value E1.
containsError (if E1 E2 E3) E :- containsError E1 E.
containsError (succ E1) E :- containsError E1 E.
containsError (pred E1) E :- containsError E1 E.
containsError (isZero E1) E :- containsError E1 E.
containsError (pair E1 E2) E :- containsError E1 E.
containsError (pair E1 E2) E :- containsError E2 E, value E1.
containsError (fst E1) E :- containsError E1 E.
containsError (snd E1) E :- containsError E1 E.
containsError (inr E1 E2) E :- containsError E2 E.
containsError (inl E1 E2) E :- containsError E2 E.
containsError (case E1 E2 E3) E :- containsError E1 E.
containsError (cons E1 E2 E3) E :- containsError E2 E.
containsError (cons E1 E2 E3) E :- containsError E3 E, value E2.
containsError (head E1 E2) E :- containsError E2 E.
containsError (tail E1 E2) E :- containsError E2 E.
containsError (isnil E1 E2) E :- containsError E2 E.
containsError (appT E1 E2) E :- containsError E1 E.
containsError (fold E1 E2) E :- containsError E1 E.
containsError (unfold E1) E :- containsError E1 E.
containsError (let E1 E2) E :- containsError E1 E.
containsError (fix E1) E :- containsError E1 E.
containsError (raise E1 E2) E :- containsError E2 E.

consistency X1 X2 :- join2 X1 X2 JoinX.
join2 (dyn) X X.
join2 X (dyn) X.
join2 (mu X1) (mu Y1) (mu Z1) :- (pi x\ join2 (X1 x) (Y1 x) (Z1 x)).
join2 (all X1) (all Y1) (all Z1) :- (pi x\ join2 (X1 x) (Y1 x) (Z1 x)).
join2 (list X1) (list Y1) (list Z1) :- join2 X1 Y1 Z1.
join2 (sum X1 X2) (sum Y1 Y2) (sum Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (times X1 X2) (times Y1 Y2) (times Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (bool) (bool) (bool).
join2 (int) (int) (int).
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (dyn) (dyn) (dyn).
ground (mu (x\ (dyn))).
ground (all (x\ (dyn))).
ground (list (dyn)).
ground (sum (dyn) (dyn)).
ground (times (dyn) (dyn)).
ground (bool).
ground (int).
ground (arrow (dyn) (dyn)).
value (cast V G L (dyn)) :- value V, ground G.
value (cast V (mu T1) L (mu T1')) :- value V.
value (cast V (all T1) L (all T1')) :- value V.
value (cast V (list T1) L (list T1')) :- value V.
value (cast V (sum T1 T2) L (sum T1' T2')) :- value V.
value (cast V (times T1 T2) L (times T1' T2')) :- value V.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
typeOfCC (fold E R) (mu R) :- typeOfCC E (R (mu R)).
typeOfCC (absT R2) (all R) :- (pi x\ typeOfCC (R2 x) (R x)).
typeOfCC (cons T E1 E2) (list T) :- typeOfCC E1 T, typeOfCC E2 (list T).
typeOfCC (emptyList T) (list T).
typeOfCC (inr T1 E) (sum T1 T2) :- typeOfCC E T2.
typeOfCC (inl T2 E) (sum T1 T2) :- typeOfCC E T1.
typeOfCC (pair E1 E2) (times T1 T2) :- typeOfCC E1 T1, typeOfCC E2 T2.
typeOfCC (ff) (bool).
typeOfCC (tt) (bool).
typeOfCC (succ E) (int) :- typeOfCC E (int).
typeOfCC (zero) (int).
typeOfCC (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (R x) T2)).
typeOfCC (unfold E) (R (mu R)) :- typeOfCC E (mu R).
typeOfCC (appT E T) (R T) :- typeOfCC E (all R).
typeOfCC (isnil T E) (bool) :- typeOfCC E (list T).
typeOfCC (tail T E) (list T) :- typeOfCC E (list T).
typeOfCC (head T E) T :- typeOfCC E (list T).
typeOfCC (case EE R1 R2) T :- typeOfCC EE (sum T1 T2), (pi x\ (typeOfCC x T1 => typeOfCC (R1 x) T)), (pi x\ (typeOfCC x T2 => typeOfCC (R2 x) T)).
typeOfCC (snd E) T2 :- typeOfCC E (times T1 T2).
typeOfCC (fst E) T1 :- typeOfCC E (times T1 T2).
typeOfCC (if E1 E2 E3) T :- typeOfCC E1 (bool), typeOfCC E2 T, typeOfCC E3 T.
typeOfCC (isZero E) (bool) :- typeOfCC E (int).
typeOfCC (pred E) (int) :- typeOfCC E (int).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (let E R) T2 :- typeOfCC E T1, (pi x\ (typeOfCC x T1 => typeOfCC (R x) T2)).
typeOfCC (letrec T1 R1 R2) T2 :- (pi x\ (typeOfCC x T1 => typeOfCC (R1 x) T1)), (pi x\ (typeOfCC x T1 => typeOfCC (R2 x) T2)).
typeOfCC (fix E) T :- typeOfCC E (arrow T T).
typeOfCC (try E1 E2) T :- typeOfCC E1 T, typeOfCC E2 (arrow (int) T).
typeOfCC (raise T E) T :- typeOfCC E (int).
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1, consistency T1 T2.
typeOfCC (blame T L) T.
getGroundOf (mu X1) (mu (x\ (dyn))).
getGroundOf (all X1) (all (x\ (dyn))).
getGroundOf (list X1) (list (dyn)).
getGroundOf (sum X1 X2) (sum (dyn) (dyn)).
getGroundOf (times X1 X2) (times (dyn) (dyn)).
getGroundOf (bool) (bool).
getGroundOf (int) (int).
getGroundOf (arrow X1 X2) (arrow (dyn) (dyn)).
sameGround T1 T2 :- getGroundOf T1 X, getGroundOf T2 X.
contains (cast E1 T1 L T2) E :- contains E1 E.
containsError (cast E1 T1 L T2) E :- containsError E1 E.
step (cast E T1 L T2) (cast E' T1 L T2) :- step E E'.
stepC (cast V T L T) V :- value V.
stepC (cast (cast V G L1 (dyn)) (dyn) L2 G) V :- value V, ground G.
stepC (cast (cast V G1 L1 (dyn)) (dyn) L2 G2) (blame G2 L2) :- value V, ground G1, ground G2, not (sameGround G1 G2).
stepC (cast V T L (dyn)) (cast (cast V T L G) G L (dyn)) :- value V, getGroundOf T G, not (ground T).
stepC (cast V (dyn) L T) (cast (cast V (dyn) L G) G L T) :- value V, getGroundOf T G, not (ground T).
stepC E (blame T1 L) :- typeOfCC E T1, contains E (blame T2 L), not (E is (blame T2 L)).
stepC (app (cast V (arrow T1' T2') L (arrow T1 T2)) E2) (cast (app V (cast E2 T1 L T1')) T2' L T2) :- value V, value E2.
stepC (fst (cast V (times T1' T2') L (times T1 T2))) (cast (fst V) T1' L T1) :- value V.
stepC (snd (cast V (times T1' T2') L (times T1 T2))) (cast (snd V) T2' L T2) :- value V.
stepC (case (cast V (sum T1' T2') L (sum T1 T2)) R1 R2) (case V (x\ (R1 (cast x T1' L T1))) (x\ (R2 (cast x T2' L T2)))) :- value V.
stepC (head T (cast V (list T') L (list T))) (cast (head T' V) T' L T) :- value V.
stepC (tail T (cast V (list T') L (list T))) (cast (tail T' V) (list T') L (list T)) :- value V.
stepC (isnil T (cast V (list T') L (list T))) (isnil T' V) :- value V.
stepC (appT (cast V (all R') L (all R)) T) (cast (appT V T) (R' T) L (R T)) :- value V.
stepC (unfold (cast V (mu R') L (mu R))) (cast (unfold V) (R' (mu R')) L (R (mu R))) :- value V.
step E E' :- stepC E E'.
