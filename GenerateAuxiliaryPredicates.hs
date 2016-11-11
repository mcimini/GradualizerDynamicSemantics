module GenerateAuxiliaryPredicates where

import System.IO.Unsafe 
import Data.Unique
import Data.Char
import Data.List
import TypeSystem
--import Static
import CastTemplate 

simples :: Int -> [TypeEntry]
simples n = map (\i -> Simple "typ") [1 .. n]

typeEntry_onlyTypes :: TypeEntry -> Bool
typeEntry_onlyTypes = (\x -> case x of { (Simple typ) -> typ == kindOftype ; (Abs var typ) -> typ == kindOftype ; otherwise -> False })

toApplication = \term -> (Application term (Bound "x"))

auxiliaryPred :: TypeSystem -> TypeSystem
auxiliaryPred ts@(Ts sig rules) = (Ts (sig ++ (sigOfNewThings ts)) (rules ++ (rulesOfAuxiliaryPred ts)))

sigOfNewThings :: TypeSystem -> Signature
sigOfNewThings (Ts sig rules) =  (nub (addConsistency ++ addJoin)) ++ addContains ++ addContainsError
                   where
                   addConsistency = [Decl consistency "o" (Left []) [Simple "typ", Simple "typ"]]
                   addJoin = [Decl "join2" "o" (Left []) [Simple "typ", Simple "typ",Simple "typ"]] ++ map (\n -> Decl ("join" ++ (show n)) "o" (Left []) (simples (n+1))) [2 .. (maximum (map neededJoins rules))]
                   addContains = [Decl "contains" "o" (Left []) [Simple "term", Simple "term"]]
                   addContainsError = [Decl "containsError" "o" (Left []) [Simple "term", Simple "term"]]
				   
matchingDeclByDecl :: SignatureEntry -> SignatureEntry
matchingDeclByDecl (Decl c typ info arguments) = (Decl ("match" ++ (firstCharUP c)) "o" (Left []) (Simple(typ):arguments)) -- (Decl ("match" ++ (firstCharUP c)) "o" (Left []) (simples ((length arguments)+1)))

rulesOfAuxiliaryPred :: TypeSystem -> [Rule]
rulesOfAuxiliaryPred (Ts sig rules) = (joinAndFlow (Ts sig rules)) ++ map joinForConstructors (listOfTypes sig) 

--matchingRulesByDecl :: SignatureEntry -> [Rule]
--matchingRulesByDecl (Decl c typ info arguments) = let newVars = map (\n -> (Var ("T" ++ (show n)))) [1 .. (length arguments)] in [Rule [] (Formula "match" [c] [(Constructor c newVars)] newVars), Rule [] (Formula "match" [c] [dyntype] (replicate (length arguments) dyntype))]

matchingRulesByDecl :: SignatureEntry -> [Rule]
matchingRulesByDecl (Decl c typ contraInfo arguments) = if not (typ == "typ") then [] else
	let newVars = getNewvars "X" (length arguments) in 
		[Rule [] (Formula "match" [c] [(Constructor c newVars)] newVars)] ++ [Rule [] (Formula "match" [c] [dyntype] (mapi (groundFor newVars) arguments))] 
	where
		groundFor newvars entry i= case entry of 
			(Simple "typ") -> dyntype
--			(Abs var "typ")  -> (Lambda "x" dyntype) 
			(Abs var ee)  -> (Lambda "x" (groundFor newvars (Simple ee) i)) 
			otherwise  -> newvars !! i

{- This is for when you want to deal with types that accepts arguments that are not types. like records. 
(Formula "match" [c] [dyntype] (mapi dynamicsOnlyForTypes arguments))]
dynamicsOnlyForTypes :: Int -> TypeEntry -> Term
dynamicsOnlyForTypes i typeentry= case typeentry of (Simple typ) -> if typ == kindOftype then dyntype else (Var ("X" ++ (show i)))
-}

flowRule :: Rule 
flowRule = (Rule
            [Formula "join" ["2"] [(Var "X1"), Var "X2"] [Var "JoinX"]]
            (Formula consistency [] [(Var "X1"), Var "X2"] []))

joinAndFlow :: TypeSystem -> [Rule]
joinAndFlow (Ts sig rules) = nub (addFlow ++ addJoin) -- nub is to not duplicate the rules for join2 that can be inserted both because of flow or joins.
            where
            addFlow = flowRule:(joinNary 2) 
            addJoin = if (maximum (map neededJoins rules)) > 1 then (joinNary (maximum (map neededJoins rules))) ++ (map joinForConstructors (listOfTypes sig)) else []

joinNary :: Int -> [Rule]
joinNary 2 = [ (Rule [] (Formula "join" ["2"] [dyntype, Var "X"] [Var "X"])),
               (Rule [] (Formula "join" ["2"] [Var "X", dyntype] [Var "X"]))]
 --              (Rule [] (Formula "join" ["2"] [Var "X", Var "X"] [Var "X"]))]

joinNary n = if (n >= 3) then (currentJoinRule:joinNary (n - 1)) else [] -- case n is 0. case n is 1 is impossible
         where
         newvars = map (\n -> Var ("X" ++ (show n))) [1 .. n]
         currentJoinRule = (Rule
                            [Formula "join" ["2"] [Var "X1", Var "X2"] [Var "Xtmp"],
                             (Formula "join" [show (n - 1)] ((Var "Xtmp"):(drop 2 newvars)) [Var "JoinX"])
                            ] 
                            (Formula "join" [(show n)] newvars [Var "JoinX"]))


joinForConstructors :: SignatureEntry -> Rule
joinForConstructors (Decl c typ info arguments) = Rule premises conclusion
         where 
			 newVarsX = map (\n -> Var ("X" ++ (show n))) [1 .. (length (arguments))]
			 newVarsY = map (\n -> Var ("Y" ++ (show n))) [1 .. (length (arguments))]
			 newVarsZ = map (\n -> Var ("Z" ++ (show n))) [1 .. (length (arguments))]
			 conclusion = Formula "join" ["2"] [Constructor c newVarsX, Constructor c newVarsY] [Constructor c newVarsZ] 
			 premises = map singleJoinPremise (findIndices typeEntry_onlyTypes arguments)
			 singleJoinPremise = \index -> case arguments !! index of 
				 (Simple typ) -> Formula "join" ["2"] [newVarsX !! index, newVarsY !! index] [newVarsZ !! index]
				 (Abs var ee) -> Generic (Formula "join" ["2"] (map toApplication [newVarsX !! index, newVarsY !! index]) (map toApplication [newVarsZ !! index]))
	             

-- First check if premises is null, or maximum will throw exception 
neededJoins :: Rule -> Int
neededJoins (Rule premises conclusion) = if (null premises) then 0 else (maximum (map neededJoins_prem premises)) where neededJoins_prem = \prem -> case prem of {(Formula pred strings interms outterms) -> if pred == "join" then read (head strings)::Int else 0 ; otherwise -> 0}

flowNeededIn :: Rule -> Bool
flowNeededIn (Rule premises conclusion) = any (predicateIn consistency) premises

predicateIn :: String -> Premise -> Bool
predicateIn pred1 prem = case prem of {(Formula pred2 strings interms outterms) -> pred1 == pred2 ; otherwise -> False}
