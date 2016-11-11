module CastManagement where

import System.IO.Unsafe 
import Data.Unique
import Data.List
import Data.String.Utils
import TypeSystem
import DestinationType
import CastInsertion
import CastTemplate as CastTemplate

addCastManagement :: TypeSystem -> TypeSystem -> TypeSystem
addCastManagement ts@(Ts sig rules) tsToAugment = 
	let augmented = (extend tsToAugment (CastTemplate.getTemplate ts)) in 
	 augmented `addRules` (map (castForOperators ts augmented) (onlyEliminatorsOfHigher ts) ++ (stepUpTostepCRule sig))
--renameToStepTickedIFNECESSARY

-- Only deconstructors of higher order types ()
castForOperators :: TypeSystem -> TypeSystem -> SignatureEntry -> Rule
castForOperators tsOrigin (Ts sig rules) (Decl c typ info entries) = 
	case info of 
		Left labels -> error (show (Decl c typ info entries))
		Right n -> case (searchRuleByPredAndName (Ts sig rules) typeOfCC c) of -- n is the eliminating argument
		 rule@(Rule premises conclusion) -> case conclusion of 
			(Formula pred strings interms outterms) -> 
			 let targetCast = extractEliminatedType rule n -- error $ show $ 
			 in let sourceCast = (enc "" targetCast) 
			 in let castValue = makeCastTerm (Var "V") sourceCast targetCast
			 in let flows = flowExtraction targetCast sourceCast -- For type preservation, we need the reverse flow
			 in let pkgSiblings = (makePkg sig (Rule (premises ++ flows) conclusion)) 
			 in let injectedTheCastValue = replaceEliminatingArgument n (head interms) (makeCastTerm (Var "V") sourceCast targetCast)
			 in let termWithNewTypeAnnotations = replaceTypeAnnotationWith pkgSiblings n injectedTheCastValue
			 in let newAssignedType = (destinationType pkgSiblings (head outterms)) 
			 in let newConclusion = (Formula pred strings [termWithNewTypeAnnotations] [newAssignedType]) 
			 in let castedOnlySiblings = rule_getFirstOutput (castInsertion_rule sig (Rule ((map (castTypesInEnvironment targetCast) premises) ++ flows) newConclusion))
			 in let castValueLiberated = replaceEliminatingArgument n castedOnlySiblings (Var "V")
			 in let flowsReversed = flowExtraction sourceCast targetCast -- Notice we flip source/target w.r.t. earlier.
			 in let pkgConclusion = (makePkg sig (Rule flowsReversed conclusion)) 
			 in let targetAssignedType = (destinationType pkgConclusion newAssignedType)
			 in let finalTargetOfReduction = makeCastTerm castValueLiberated newAssignedType targetAssignedType
			 -- we add the cast eliminated argument later because cast insertion handles only variables.
			 in let valuePremises = valuehoodInsertion tsOrigin injectedTheCastValue
			 in 
			 (Rule 
			 	([(Formula "value" [] [(Var "V")] [])] ++ valuePremises)
				(Formula step [] [injectedTheCastValue] [finalTargetOfReduction])
			 )	

castTypesInEnvironment :: Term -> Premise -> Premise
castTypesInEnvironment canonical premise = case premise of 
	(Hypothetical (Formula pred1 strings1 interms1 [typ]) (Formula pred2 strings2 [Application var applied] outterms2)) -> 
		if elem typ (varsOf canonical) 
			then (Hypothetical (Formula pred1 strings1 interms1 [typ]) (Formula pred2 strings2 [Application var (makeCastTerm applied (enc "" typ) typ)] outterms2)) 
			else premise 
	otherwise -> premise 

flowExtraction :: Term -> Term -> [Premise]
flowExtraction (Constructor c1 terms1) (Constructor c2 terms2) = (zipWith makeFlow terms1 terms2) 

extractCanonicalType :: Rule -> [Premise] -> Term
extractCanonicalType cc [] = error ("ERROR extractCanonicalType: I did not find a deconstructed type in the rule" ++ (show cc))
extractCanonicalType cc (premise:rest) = case premise of (Formula pred info interms outterms) -> if pred == typeOfCC then case (head outterms) of { (Constructor c terms) -> (Constructor c terms) ; otherwise -> extractCanonicalType cc rest } else extractCanonicalType cc rest

flowPremisesFromCasts :: Package -> String -> Term -> [Premise]
flowPremisesFromCasts pkg c_app canonical = 
 case canonical of (Constructor c_arrow arguments) -> let encArguments = map (enc "") arguments in zipWith makeFlow arguments encArguments ++ zipWith makeFlow encArguments arguments

replaceTypeAnnotationWith :: Package -> Int -> Term -> Term
replaceTypeAnnotationWith pkg n (Constructor c_arrow arguments) = (Constructor c_arrow $ replaceAtIndex (n-1) (arguments !! (n-1)) (map (destinationType pkg) arguments))
--	let numero = numberOfTypeAnnotations (pkg_getSig pkg) c in let newTerms = (map (destinationType pkg) (take numero terms)) ++ (drop numero terms) in (Constructor c newTerms) destinationType pkg term = 

stepUpTostepCRule :: Signature -> [Rule] 
stepUpTostepCRule sig = case searchDeclByName sig "step" of 
	Nothing -> error "ERROR: There is no predicate step"
	Just (Decl c typ info entries) -> let n = length entries in if n > 0 && entries !! 0 == (Simple "term") 
		then let numberOfInputs = (n `div` 2) in let newvars = tail $ getNewvars "X" numberOfInputs  
			in [(Rule [Formula step [] [Var "E"] [Var "E'"]] (Formula "step" [] ((Var "E"):newvars) ((Var "E'"):newvars)))] 
		else error "ERROR: The first argument of step must be an expression. Like step E1 E2 or step E1 MU1 E2 MU2"

valuehoodInsertion :: TypeSystem -> Term -> [Premise]
valuehoodInsertion ts@(Ts sig rules) (Constructor c args) = 
--	if length (filter (filterRules "step") rules) < 2 then [] else -- here is when the language is up-to, like stepRef E Mu E' Mu' and doesn't have step. 
	concat (map indexesOfValues (searchRuleByPredAndNameList ts "step" c))
	 where 
	  indexesOfValues (Rule premises conclusion) = case (getFirstInputOfPremise conclusion) of (Constructor c2 args2) -> map toValue (filter (\x -> x>0) (mapi (checkValuehood premises) args2))
	  toValue i = (Formula "value" [] [args !! i] [])
	  checkValuehood premises arg i = if i == 0 then 0 else case searchPremiseByPredAndVar_prem premises "value" arg of 
		  Nothing -> 0
		  Just something -> i 
