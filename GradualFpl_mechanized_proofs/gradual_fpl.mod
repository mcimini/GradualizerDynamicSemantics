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

step (case (inl T E1) R1 R2) (R1 E1) :- value E1.

step (case (inr T E1) R1 R2) (R2 E1) :- value E1.

typeOf (snd E) T2 :- typeOf E (times T1 T2).

step (snd (pair E1 E2)) E2 :- value E1, value E2.

typeOf (fst E) T1 :- typeOf E (times T1 T2).

step (fst (pair E1 E2)) E1 :- value E1, value E2.

typeOf (if E1 E2 E3) T :- typeOf E1 (bool ), typeOf E2 T, typeOf E3 T.

step (if (tt ) E1 E2) E1.

step (if (ff ) E1 E2) E2.

typeOf (isZero E) (bool ) :- typeOf E (int ).

step (isZero (zero )) (tt ).

step (isZero (succ E)) (ff ) :- value E.

typeOf (pred E) (int ) :- typeOf E (int ).

step (pred (zero )) (raise int (zero )).

step (pred (succ E)) E :- value E.

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

value (inr T E1) :- value E1.

value (inl T E1) :- value E1.

value (pair E1 E2) :- value E1, value E2.

value (ff ).

value (tt ).

value (succ E1) :- value E1.

value (zero ).

value (abs T1 R2).

step (fold E1 U2) (fold E1' U2) :- step E1 E1'.

step (cons T1 E2 E3) (cons T1 E2' E3) :- step E2 E2'.

step (cons T1 E2 E3) (cons T1 E2 E3') :- step E3 E3', value E2.

step (inr T E1) (inr T E1') :- step E1 E1'.

step (inl T E1) (inl T E1') :- step E1 E1'.

step (pair E1 E2) (pair E1' E2) :- step E1 E1'.

step (pair E1 E2) (pair E1 E2') :- step E2 E2', value E1.

step (succ E1) (succ E1') :- step E1 E1'.

step (unfold E1) (unfold E1') :- step E1 E1'.

step (appT E1 T2) (appT E1' T2) :- step E1 E1'.

step (isnil T1 E2) (isnil T1 E2') :- step E2 E2'.

step (tail T1 E2) (tail T1 E2') :- step E2 E2'.

step (head T1 E2) (head T1 E2') :- step E2 E2'.

step (case E1 R2 R3) (case E1' R2 R3) :- step E1 E1'.

step (snd E1) (snd E1') :- step E1 E1'.

step (fst E1) (fst E1') :- step E1 E1'.

step (if E1 E2 E3) (if E1' E2 E3) :- step E1 E1'.

step (isZero E1) (isZero E1') :- step E1 E1'.

step (pred E1) (pred E1') :- step E1 E1'.

step (app E1 E2) (app E1' E2) :- step E1 E1'.

step (app E1 E2) (app E1 E2') :- step E2 E2', value E1.

step (let E1 R) (let E1' R) :- step E1 E1'.

step (fix E1) (fix E1') :- step E1 E1'.

step (try E1 E2) (try E1' E2) :- step E1 E1'.

step (raise T E1) (raise T E1') :- step E1 E1'.

error (raise T V) :- value V. 

typeOf (raise T E) T :- typeOf E (int ).

step E (raise T1 V) :- typeOfCC E T1, containsError E (raise T2 V), value V. 

nstep E E.

nstep E1 E3 :- step E1 E2, nstep E2 E3.

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
contains (inr T E1) E :- contains E1 E.
contains (inl T E1) E :- contains E1 E.
contains (case E1 E2 E3) E :- contains E1 E.
contains (cons E1 E2 E3) E :- contains E2 E.
contains (cons E1 E2 E3) E :- contains E3 E, value E2.
contains (head E1 E2) E :- contains E2 E.
contains (tail E1 E2) E :- contains E2 E.
contains (isnil E1 E2) E :- contains E2 E.
contains (appT E1 E2) E :- contains E1 E.
contains (fold E1 E2) E :- contains E1 E.
contains (unfold E1) E :- contains E1 E.
contains (let E1 R) E :- contains E1 E.
contains (fix E1) E :- contains E1 E.
contains (try E1 E2) E :- contains E1 E.
contains (raise T E1) E :- contains E1 E.

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
containsError (inr T E1) E :- containsError E1 E.
containsError (inl T E1) E :- containsError E1 E.
containsError (case E1 E2 E3) E :- containsError E1 E.
containsError (cons E1 E2 E3) E :- containsError E2 E.
containsError (cons E1 E2 E3) E :- containsError E3 E, value E2.
containsError (head E1 E2) E :- containsError E2 E.
containsError (tail E1 E2) E :- containsError E2 E.
containsError (isnil E1 E2) E :- containsError E2 E.
containsError (appT E1 E2) E :- containsError E1 E.
containsError (fold E1 E2) E :- containsError E1 E.
containsError (unfold E1) E :- containsError E1 E.
containsError (let E1 R) E :- containsError E1 E.
containsError (fix E1) E :- containsError E1 E.
containsError (raise T E1) E :- containsError E1 E.


flowAbs X1 X2 :- flow X1 X2, isType X1, isType X2.
flow (dyn) T :- isType T.
flow T (dyn) :- isType T.
flow (arrow X1 X2) (arrow Y1 Y2) :- flow X1 Y1, flow X2 Y2.
flow (mu R) (mu R') :- pi x\ pi y\ flowAbs (R x) (R' y).
flow (all X1) (all Y1) :- (pi x\ flow (X1 x) (Y1 x)).
flow (list X1) (list Y1) :- flow X1 Y1.
flow (sum X1 X2) (sum Y1 Y2) :- flow X1 Y1, flow X2 Y2.
flow (times X1 X2) (times Y1 Y2) :- flow X1 Y1, flow X2 Y2.
flow (bool) (bool).
flow (int) (int).
flow (dyn) (dyn).

ground (arrow (dyn) (dyn)).
ground (mu (x\ (dyn))).
ground (all (x\ (dyn))).
ground (list (dyn)).
ground (sum (dyn) (dyn)).
ground (times (dyn) (dyn)).
ground (bool).
ground (int).
value (cast V G L (dyn)) :- value V, ground G.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
value (cast V (mu T1) L (mu T1')) :- value V.
value (cast V (all T1) L (all T1')) :- value V.
value (cast V (list T1) L (list T1')) :- value V.
value (cast V (sum T1 T2) L (sum T1' T2')) :- value V.
value (cast V (times T1 T2) L (times T1' T2')) :- value V.
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
typeOfCC (let E R) T2 :- typeOfCC E T1, (pi x\ typeOfCC x T1 => typeOfCC (R x) T2).
typeOfCC (letrec T1 R1 R2) T2 :- (pi x\ (typeOfCC x T1 => typeOfCC (R1 x) T1)), (pi x\ (typeOfCC x T1 => typeOfCC (R2 x) T2)).
typeOfCC (fix E) T :- typeOfCC E (arrow T T).
typeOfCC (try E1 E2) T :- typeOfCC E1 T, typeOfCC E2 (arrow (int) T).
typeOfCC (raise T E) T :- typeOfCC E (int).
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1, flow T1 T2.
typeOfCC (blame T L) T.
contains (cast E1 T1 L T2) E :- contains E1 E.
containsError (cast E1 T1 L T2) E :- containsError E1 E.
stepC (cast V T L T) V :- value V.
stepC (cast (cast V G L1 (dyn)) (dyn) L2 G) V :- value V, ground G.
stepC (cast (cast V G1 L1 (dyn)) (dyn) L2 G2) (blame G2 L2) :- value V, ground G1, ground G2, different G1 G2.
stepC (cast V T L (dyn)) (cast (cast V T L G) G L (dyn)) :- value V, getGroundOf T G, different_hi T G.  
stepC (cast V (dyn) L T) (cast (cast V (dyn) L G) G L T) :- value V, getGroundOf T G, different_hi T G. 
getGroundOf (arrow X1 X2) (arrow (dyn) (dyn)).
getGroundOf (mu X1) (mu (x\ (dyn))).
getGroundOf (all X1) (all (x\ (dyn))).
getGroundOf (list X1) (list (dyn)).
getGroundOf (sum X1 X2) (sum (dyn) (dyn)).
getGroundOf (times X1 X2) (times (dyn) (dyn)).
stepC (app (cast V (arrow T1' T2') L (arrow T1 T2)) E2) (cast (app V (cast E2 T1 L T1')) T2' L T2) :- value V, value E2.
stepC (fst (cast V (times T1' T2') L (times T1 T2))) (cast (fst V) T1' L T1) :- value V.
stepC (snd (cast V (times T1' T2') L (times T1 T2))) (cast (snd V) T2' L T2) :- value V.
stepC (case (cast V (sum T1' T2') L (sum T1 T2)) R1 R2) (case V (x\ (R1 (cast x T1' L T1))) (x\ (R2 (cast x T2' L T2)))) :- value V.
stepC (head T (cast V (list T') L (list T''))) (cast (head T' V) T' L T'') :- value V.
stepC (tail T (cast V (list T') L (list T''))) (cast (tail T' V) (list T') L (list T'')) :- value V.
stepC (isnil T (cast V (list T') L (list T''))) (isnil T' V) :- value V.
stepC (appT (cast V (all R') L (all R)) T) (cast (appT V T) (R' T) L (R T)) :- value V.
stepC (unfold (cast V (mu R') L (mu R))) (cast (unfold V) (R' (mu R')) L (R (mu R))) :- value V.
step (cast E T1 L T2) (cast E' T1 L T2) :- step E E'.
step E E' :- stepC E E'.
step E (blame T1 L) :- typeOfCC E T1, contains E (blame T2 L).

different_hi T1 T2 :- different T1 T2.
different_hi (arrow T1 T2) (arrow T1' T2') :-  different T1 T1'. 
different_hi (arrow T1 T2) (arrow T1' T2') :-  different T2 T2'. 
different_hi (times T1 T2) (times T1' T2') :-  different T1 T1'. 
different_hi (times T1 T2) (times T1' T2') :-  different T2 T2'. 
different_hi (sum T1 T2) (sum T1' T2') :-  different T1 T1'. 
different_hi (sum T1 T2) (sum T1' T2') :-  different T2 T2'. 
different_hi (list T1) (list T1') :-  different T1 T1'. 
different_hi (all R) (all R') :-  pi x\ different (R x) (R' x). 
different_hi (mu R) (mu R') :-  pi x\ different (R x) (R' x). 

different dyn int. different dyn bool. different dyn (arrow T1 T2). different dyn (times T1 T2). different dyn (sum T1 T2). different dyn (list T'). different dyn (all R'). different dyn (mu R'). 

different int dyn. different int bool. different int (arrow T1 T2). different int (times T1 T2). different int (sum T1 T2). different int (list T'). different int (all R'). different int (mu R'). 

different bool dyn. different bool int. different bool (arrow T1 T2). different bool (times T1 T2). different bool (sum T1 T2). different bool (list T'). different bool (all R'). different bool (mu R'). 

different (arrow T1 T2) dyn. different (arrow T1 T2) int. different (arrow T1 T2) bool. different (arrow T1 T2) (times T1' T2'). different (arrow T1 T2) (sum T1' T2'). different (arrow T1 T2) (list T'). different (arrow T1 T2) (all R'). different (arrow T1 T2) (mu R'). 

different (times T1 T2) dyn. different (times T1 T2) int. different (times T1 T2) bool. different (times T1 T2) (arrow T1' T2'). different (times T1 T2) (sum T1' T2'). different (times T1 T2) (list T'). different (times T1 T2) (all R'). different (times T1 T2) (mu R'). 

different (sum T1 T2) dyn. different (sum T1 T2) int. different (sum T1 T2) bool. different (sum T1 T2) (arrow T1' T2'). different (sum T1 T2) (times T1' T2'). different (sum T1 T2) (list T'). different (sum T1 T2) (all R'). different (sum T1 T2) (mu R'). 

different (list T') dyn. different (list T') int. different (list T') bool. different (list T') (arrow T1' T2'). different (list T') (times T1' T2'). different (list T') (sum T1' T2'). different (list T') (all R'). different (list T') (mu R'). 

different (all R') dyn. different (all R') int. different (all R') bool. different (all R') (arrow T1' T2'). different (all R') (times T1' T2'). different (all R') (sum T1' T2'). different (all R') (list T'). different (all R1) (mu R2). 

different (mu R') dyn. different (mu R') int. different (mu R') bool. different (mu R') (arrow T1' T2'). different (mu R') (times T1' T2'). different (mu R') (sum T1' T2'). different (mu R') (list T'). different (mu R1) (all R2). 

result E :- value E.  
result (blame T L).

subtype (int) (int).
subtype (bool) (bool).
subtype (dyn) (dyn).
subtype (arrow T1' T2') (arrow T1 T2) :- subtype T1 T1', subtype T2' T2. 
subtype (times T1' T2') (times T1 T2) :- subtype T1' T1, subtype T2' T2. 
subtype (sum T1' T2') (sum T1 T2) :- subtype T1' T1, subtype T2' T2. 
subtype (list T') (list T) :- subtype T' T.
subtype (all R') (all R) :- pi x\ subtype (R' x) (R x).
subtype (mu R') (mu R) :-  pi x\ pi y\ subtype x y => subtype (R' x) (R y).
subtype T (dyn) :- subtype T G, ground G.

isType (int).
isType (bool).
isType (dyn).
isType (arrow T1 T2) :- isType T1, isType T2. 
isType (times T1 T2) :- isType T1, isType T2. 
isType (sum T1 T2) :- isType T1, isType T2. 
isType (list T) :- isType T. 
isType (all R) :- pi x\ isType (R x). 
isType (mu R) :- pi x\ isType (R x). 

lessPreciseType (int) (int).
lessPreciseType (bool) (bool).
lessPreciseType (dyn) T. 
lessPreciseType (arrow T1' T2') (arrow T1 T2) :- lessPreciseType T1' T1, lessPreciseType T2' T2. 
lessPreciseType (times T1' T2') (times T1 T2) :- lessPreciseType T1' T1, lessPreciseType T2' T2. 
lessPreciseType (sum T1' T2') (sum T1 T2) :- lessPreciseType T1' T1, lessPreciseType T2' T2. 
lessPreciseType (list T') (list T) :- lessPreciseType T' T.
lessPreciseType (all R') (all R) :- pi x\ pi y\ lessPreciseType x y => lessPreciseType (R' x) (R y).
lessPreciseType (mu R') (mu R) :-  pi x\ pi y\ lessPreciseType x y => lessPreciseType (R' x) (R y).

lessPrecise tt tt.
lessPrecise ff ff.
lessPrecise zero zero.
lessPrecise (succ E1) (succ E1') :- lessPrecise E1 E1'.
lessPrecise (abs T R) (abs T' R') :- lessPreciseType T T', pi x\ pi y\ lessPrecise x y => lessPrecise (R x) (R' y).
lessPrecise (pair E1 E2) (pair E1' E2') :- lessPrecise E1 E1', lessPrecise E2 E2'.
lessPrecise (inl T E1) (inl T' E1') :- lessPrecise E1 E1', lessPreciseType T T'.
lessPrecise (inr T E1) (inr T' E1') :- lessPrecise E1 E1', lessPreciseType T T'.
lessPrecise (emptyList T) (emptyList T') :- lessPreciseType T T'.
lessPrecise (cons T E1 E2) (cons T' E1' E2') :- lessPreciseType T T', lessPrecise E1 E1', lessPrecise E2 E2'.
lessPrecise (absT R) (absT R') :- pi x\ pi y\ lessPreciseType x y => lessPrecise (R x) (R' y).
lessPrecise (fold E1 R) (fold E1' R') :- lessPrecise E1 E1', pi x\ pi y\ lessPreciseType x y => lessPreciseType (R x) (R' y). %pi x\ lessPreciseType (R x) (R' x).
lessPrecise (app E1 E2) (app E1' E2') :- lessPrecise E1 E1', lessPrecise E2 E2'.
lessPrecise (if E1 E2 E3) (if E1' E2' E3') :- lessPrecise E1 E1', lessPrecise E2 E2', lessPrecise E3 E3'.
lessPrecise (pred E1) (pred E1') :- lessPrecise E1 E1'.
lessPrecise (isZero E1) (isZero E1') :- lessPrecise E1 E1'.
lessPrecise (fst E1) (fst E1') :- lessPrecise E1 E1'.
lessPrecise (snd E1) (snd E1') :- lessPrecise E1 E1'.
lessPrecise (case E1 R1 R2) (case E1' R1' R2') :- 
lessPrecise E1 E1', typeOfCC E1 (sum T3 T4), typeOfCC E1' (sum T1 T2), pi x\ pi y\ lessPrecise x y => typeOfCC x T3 => typeOfCC y T1 => lessPrecise (R1 x) (R1' y), pi x\ pi y\ lessPrecise x y => typeOfCC x T4 => typeOfCC y T2 => lessPrecise (R2 x) (R2' y).
lessPrecise (head T E2) (head T' E2') :- lessPreciseType T T', lessPrecise E2 E2'.
lessPrecise (tail T E2) (tail T' E2') :- lessPreciseType T T', lessPrecise E2 E2'.
lessPrecise (isnil T E2) (isnil T' E2') :- lessPreciseType T T', lessPrecise E2 E2'.
lessPrecise (appT E1 T) (appT E1' T') :- lessPrecise E1 E1', lessPreciseType T T'.
lessPrecise (unfold E1) (unfold E1') :- lessPrecise E1 E1'.
lessPrecise (let E1 R) (let E1' R') :- lessPrecise E1 E1', pi x\ pi y\ lessPrecise x y => lessPrecise (R x) (R' y).
lessPrecise (fix E1) (fix E1') :- lessPrecise E1 E1'.
lessPrecise (try E1 E2) (try E1' E2') :- lessPrecise E1 E1', lessPrecise E2 E2'.
lessPrecise (raise T E1) (raise T' E1') :- lessPrecise E1 E1', lessPreciseType T T'.
lessPrecise (cast E1 T1 L1 T2) (cast E1' T1' L2 T2') :- lessPrecise E1 E1', lessPreciseType T1 T1', lessPreciseType T2 T2'.
lessPrecise (cast E1 T1 L2 T2) E2 :- lessPrecise E1 E2, typeOfCC E2 T, lessPreciseType T1 T, lessPreciseType T2 T.
lessPrecise E1 (cast E2 T1 L2 T2) :- lessPrecise E1 E2, typeOfCC E1 T, lessPreciseType T T1, lessPreciseType T T2.
lessPrecise E (blame T L).
