module TypeSystem where

import Data.String.Utils
import Data.Char
import Data.List
import Data.Either

type Signature = [SignatureEntry]
type ElimInfo = Int -- 0 means not eliminator, n>0 means eliminator with eliminating argument n.
type VarianceInfo = [VarianceLabel]

data VarianceLabel = Inv 
					| Cov 
					| Contra
					deriving (Show, Eq, Ord)

data TypeEntry = Simple String
               | Abs String String
               deriving (Show, Eq, Ord)

data SignatureEntry = Decl String String (Either VarianceInfo ElimInfo) [TypeEntry]
     deriving (Show, Eq, Ord)
				

data Term = Var String
          | Constructor String [Term]
          | Application Term Term
          | Lambda String Term
          | Bound String
          deriving (Show, Eq, Ord)


data Premise = Formula String [String] [Term] [Term]
             | Hypothetical Premise Premise
             | Generic Premise
             | Negated Premise
             deriving (Show, Eq, Ord)
				

data Rule = Rule [Premise] Premise
     deriving (Show, Eq, Ord)
			

data TypeSystem = Ts Signature [Rule] 
     deriving (Show, Eq, Ord)


signatureOf :: TypeSystem -> Signature
signatureOf (Ts sig rules) = sig

extend :: TypeSystem -> TypeSystem -> TypeSystem
extend (Ts sig rules) (Ts newsig newrules) = (Ts (nub (sig ++ newsig)) (nub (rules ++ newrules)))

addRules :: TypeSystem -> [Rule] -> TypeSystem
(Ts sig rules) `addRules` newrules = (Ts sig (nub (rules ++ newrules))) 
	 
toStringT :: Term -> String
toStringT (Var variable) = variable
toStringT term = error "Error: toStringT is used for a complex term. Probably outputs does not contain variables."

toStringTE :: TypeEntry -> String
toStringTE (Simple item) = item
toStringTE term = error "Error: toStringTE is used for an abstraction. Probably the signature is ill-formed."

kindOftype :: String
kindOfterm :: String
typeOf :: String
typeOfGradual :: String
kindOftype = "typ"
kindOfterm = "term"
typeOf = "typeOf"
typeOfGradual = typeOf ++ "Gr"
typeOfCC = "typeOf" ++ "CC"
typeOfCI = "compToCC"
stepOriginal = "step"
step = "stepC"
consistency = "consistency"
flowPred = "consistency"

renamingInRule :: String -> String -> Rule -> Rule
renamingInRule pred1 pred2 (Rule premises conclusion) = (Rule (map (renamingInRule_prem pred1 pred2) premises) (renamingInRule_prem pred1 pred2 conclusion))
renamingInRule_prem :: String -> String -> Premise -> Premise
renamingInRule_prem pred1 pred2 premise = case premise of 
	Formula pred strings interms outterms -> if pred == pred1 then (Formula pred2 strings interms outterms) else premise 
	Hypothetical premise1 premise2 -> Hypothetical (renamingInRule_prem pred1 pred2 premise1) (renamingInRule_prem pred1 pred2 premise2)
	Generic premise1 -> Generic (renamingInRule_prem pred1 pred2 premise1)
	otherwise -> premise

varsOf :: Term -> [Term]
varsOf (Var variable) = [(Var variable)]
varsOf (Constructor c terms) = (concat (map varsOf terms)) 
varsOf (Application term1 term2) = varsOf term1 ++ varsOf term2
varsOf (Bound x) = []

getNewvars :: String -> Int -> [Term]	 
getNewvars char n = map (\n -> Var (char ++ (show n))) [1 .. n]

provedOperator :: Rule -> String
provedOperator (Rule premises conclusion) = case conclusion of (Formula pred1 strings interms outterms) -> case (head interms) of { (Constructor c terms) -> c ; otherwise -> error (show (Rule premises conclusion)) }

listOfTypes :: Signature -> Signature
listOfTypes sig = let onlytypes = (\x -> case x of (Decl c typ info entries) -> typ == "typ") in (filter onlytypes sig)

listOfTerms :: Signature -> Signature
listOfTerms sig = let onlyterms = (\x -> case x of (Decl c typ info entries) -> typ == "term") in (filter onlyterms sig)


onlyEliminatorsOfHigher :: TypeSystem -> Signature
onlyEliminatorsOfHigher ts@(Ts sig rules) = 
	let typeHigher c n = case extractEliminatedType (searchRuleByPredAndName ts typeOf c) n of (Constructor c terms) -> length terms > 0
    in 
	 let mysearch = (\x -> case x of { (Decl c typ (Right n) entries) -> n > 0 && typeHigher c n ; otherwise -> False }) in 
      filter mysearch sig 

searchDeclByName :: Signature -> String -> Maybe SignatureEntry
searchDeclByName [] c = Nothing
searchDeclByName (entry:rest) c = case entry of Decl c1 typ info entries -> if c1 == c then Just entry else searchDeclByName rest c 

deleteDeclByName :: Signature -> String -> Signature
deleteDeclByName [] c = []
deleteDeclByName (entry:rest) c = case entry of Decl c1 typ info entries -> if c1 == c then rest else entry:(deleteDeclByName rest c)

type Package = (Signature, Rule)

makePkg :: Signature -> Rule -> Package
makePkg sig rule = (sig, rule)

pkg_getSig :: Package -> Signature
pkg_getSig pkg = fst pkg

pkg_getRule :: Package -> Rule
pkg_getRule pkg = snd pkg

isContravariant :: Package -> String -> Bool
isContravariant pkg c = length (contravariantArguments pkg c) > 0

contravariantArguments :: Package -> String -> [Int]
contravariantArguments pkg c = 
	case searchDeclByName (pkg_getSig pkg) c of 
		Just (Decl c1 typ (Left contraLabels) entries) -> elemIndices Contra contraLabels
		otherwise -> []

-- This below is a default choice: contravariant arguments always appear all first. 
-- FlowDiscovery is simplified thanks to this. Though, I should change it. 
number_contravariantArguments :: Package -> String -> Int
number_contravariantArguments pkg c = maximum (0:(map (+ 1) (contravariantArguments pkg c)))

userTypeArguments :: Package -> String -> Int
userTypeArguments pkg c = case searchDeclByName (pkg_getSig pkg) c of { Nothing -> error "ERROR: userTypeArguments, not found" ; Just entry -> case entry of Decl c1 typ info entries -> count (\entry -> case entry of { Simple tt -> tt == kindOftype ; otherwise -> False }) entries } where count = \p -> length . filter p

isTypeConstructor :: Package -> String -> Bool
isTypeConstructor pkg c = case searchDeclByName (pkg_getSig pkg) c of { Nothing -> error ("isTypeConstructor: not found " ++ c ++ (show (pkg_getSig pkg))) ; Just entry -> case entry of Decl c1 typ info entries -> typ == kindOftype }


-- I need the addition: (n + length typeannotations) because in letrec T1 R1, R1 is the first of programs and have n = 0. The lookup of (Abs term1 term2) is compromised then.
isAbstraction :: Package -> Term -> Bool
isAbstraction pkg (Var variable) = case (pkg_getRule pkg) of (Rule premises conclusion) -> case conclusion of (Formula pred1 strings interms outterms) -> case (head interms) of (Constructor c terms) -> case (elemIndex (Var variable) terms) of { Nothing -> error (concat (map toStringT terms)) ; Just n -> case (searchDeclByName (pkg_getSig pkg) c) of { Nothing -> error $ "ERROR: isAbstraction failed to find the constructor in the signature:" ++ show (pkg_getSig pkg) ++ show c ; Just (Decl c1 typ info entries) -> case (entries !! n) of { Simple term -> False ; (Abs term1 term2) -> True } } }

isBound :: Term -> Bool
isBound (Bound name) = True
isBound other = False

searchRuleByPredAndName :: TypeSystem -> String -> String -> Rule 
searchRuleByPredAndName (Ts sig rules) pred1 c1 = 
	head (filter myrule rules) 
	 where 
		 myrule = \rule -> case rule of 
			 (Rule premises conclusion) -> case conclusion of 
				 (Formula pred2 info interms outterms) -> pred1 == pred2 && case getFirstInputOfPremise conclusion of 
					 (Constructor c2 terms) -> c1 == c2 
					 otherwise -> False

searchRuleByPredAndNameList :: TypeSystem -> String -> String -> [Rule] 
searchRuleByPredAndNameList (Ts sig rules) pred1 c1 = 
	(filter myrule rules) 
	 where 
		 myrule = \rule -> case rule of 
			 (Rule premises conclusion) -> case conclusion of 
				 (Formula pred2 info interms outterms) -> pred1 == pred2 && case getFirstInputOfPremise conclusion of 
					 (Constructor c2 terms) -> c1 == c2 
					 otherwise -> False

-- Given a predicate and a term, it returns the first premise that uses the predicate and the term as in input.
-- Example: typeOf E (T1 -> T2) is returned when searched with searchPremiseByPredAndVar rule "typeOf" (Var "E")
searchPremiseByPredAndVar :: Rule -> String -> Term -> Maybe Premise
searchPremiseByPredAndVar (Rule premises conclusion) pred variable = searchPremiseByPredAndVar_prem premises pred variable
searchPremiseByPredAndVar_prem :: [Premise] -> String -> Term -> Maybe Premise
searchPremiseByPredAndVar_prem [] pred variable = Nothing
searchPremiseByPredAndVar_prem ((Formula pred2 strings interms outterms):rest) pred1 variable = if equalOrTypeOf pred1 pred2 && elem variable (varsOf (head interms)) then Just (Formula pred2 strings interms outterms) else searchPremiseByPredAndVar_prem rest pred1 variable -- error (pred1 ++ pred2) 
searchPremiseByPredAndVar_prem ((Hypothetical premise1 premise2):rest) pred variable = searchPremiseByPredAndVar_prem (premise2:rest) pred variable
searchPremiseByPredAndVar_prem ((Generic premise):rest) pred variable = searchPremiseByPredAndVar_prem (premise:rest) pred variable


rule_getInput :: Rule -> Term 
rule_getInput (Rule premises conclusion) = getFirstInputOfPremise conclusion

rule_getFirstOutput :: Rule -> Term 
rule_getFirstOutput (Rule premises conclusion) = getFirstOutputOfPremise conclusion

getFirstInputOfPremise :: Premise -> Term
getFirstInputOfPremise (Formula pred strings interms outterms) = head interms

getFirstOutputOfPremise :: Premise -> Term
getFirstOutputOfPremise (Formula pred strings interms outterms) = head outterms

getOutputsOfPremise :: Premise -> [Term]
getOutputsOfPremise (Formula pred strings interms outterms) = outterms

equalOrTypeOf :: String -> String -> Bool
equalOrTypeOf pred1 pred2 = (pred1 == pred2) || (startswith typeOf pred1 && startswith typeOf pred2)

firstCharUP [] = []
firstCharUP (h:t) = (toUpper h):t

removePredicateFromTS :: TypeSystem -> String -> TypeSystem
removePredicateFromTS (Ts sig rules) pred = (Ts (deleteDeclByName sig pred) (filter (not . (filterRules pred)) (map (removePredicateFromRule pred) rules)))

removePredicateFromRule :: String -> Rule -> Rule
removePredicateFromRule pred1 (Rule premises conclusion) = (Rule (filter noPred premises) conclusion) where noPred = \premise -> case premise of { (Formula pred2 strings interms outterms) -> not (pred1 == pred2) ; _ -> True }

filterRules :: String -> Rule -> Bool
filterRules pred1 (Rule premises conclusion) = case conclusion of { (Formula pred2 strings interms outterms) -> (pred1 == pred2) || (tickedpred1 == pred2) ; _ -> False } where tickedpred1 = pred1 ++ "'"

numberOfTypeAnnotations :: Signature -> String -> Int
numberOfTypeAnnotations [] c = error "ERROR: numberOfTypeAnnotations. Did not find the constructor in the signature"
numberOfTypeAnnotations (entry:rest) c1 = case entry of (Decl c2 typ info entries) -> if c1 == c2 then length $ filter (istype) entries else numberOfTypeAnnotations rest c1 where istype = \entry -> entry == (Simple "typ")

replaceEliminatingArgument :: Int -> Term -> Term -> Term
replaceEliminatingArgument n (Constructor c_app args) term = (Constructor c_app args') where args' = replaceAtIndex (n-1) term args

extractEliminatedType :: Rule -> Int -> Term 
--extractEliminatedType (Rule premises conclusion) n = getFirstOutputOfPremise (premises !! (n-1))
extractEliminatedType rule n = case rule_getInput rule of 
	(Constructor c args) -> let elimininatingVar = args !! (n-1) in 
	  case searchPremiseByPredAndVar rule typeOfCC elimininatingVar of 
		  Nothing -> error $ "extractEliminatedType: I didnt find " ++ show elimininatingVar ++ show rule
		  Just premise -> getFirstOutputOfPremise premise

doesItAppear :: String -> TypeSystem -> Bool
doesItAppear c (Ts sig rules) = (any (doesItAppear_rule c) rules)-- (filter (filterRules "step") rules)) && (case searchDeclByName sig c of { Nothing -> True ; Just something -> False} )
doesItAppear_rule c (Rule premises conclusion) = case conclusion of (Formula pred strings interms outterms) -> pred == c || any (doesItAppear_trm c) outterms
doesItAppear_trm c1 (Constructor c2 terms) = if c1 == c2 then True else any (doesItAppear_trm c1) terms
doesItAppear_trm c1 (Application term1 term2) = any (doesItAppear_trm c1) [term1,term2]
doesItAppear_trm c1 (Lambda bound term) = any (doesItAppear_trm c1) [term]
doesItAppear_trm c1 otherwise = False

mapi f l = zipWith f l [0..]
replaceAtIndex n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceAtIndex (n-1) newVal xs
	 
fstOf3 (a,b,c) = a
sndOf3 (a,b,c) = b
thirdOf3 (a,b,c) = c
