module GenerateCalculi where

import System.IO
import System.IO.Unsafe
import Data.Unique
import Data.List
import Data.Maybe
import TypeSystem
import CastInsertion
import GenerateAuxiliaryPredicates
import CastManagement
import ToLambdaProlog
import FromLambdaProlog
import Library

getRidOfNone :: [Maybe Rule] -> [Rule]
getRidOfNone maybelist = map fromJust $ filter isJust maybelist

completeRules :: TypeSystem -> ([(String, [(Int, [Int])])] , [String]) -> [Rule]
completeRules (Ts sig rules) (contexts, errorHandlers) = (map fstOf3 tripleOfRulesContextAndContainsAndError) 
    ++ [Rule [] (Formula "contains" [] [Var "E", Var "E"] [])] ++ (map sndOf3 tripleOfRulesContextAndContainsAndError) 
    ++ if errorHandlers == [] then [] else [Rule [] (Formula "containsError" [] [Var "E", Var "E"] [])] ++ getRidOfNone (map thirdOf3 tripleOfRulesContextAndContainsAndError) 
	
	where 
		third triplette = snd (snd triplette)
		tripleOfRulesContextAndContainsAndError = concat $ map contextForEach contexts 
		getValue n = Formula "value" [] [Var ("E" ++ show n)] []
		toProgressStep n = Formula "step" [] [Var ("E" ++ show n)] [Var ("E" ++ show n ++ "'")]
		toContains containPred n = Formula containPred [] [Var ("E" ++ show n)] [Var "E"]
		contextForEach cAndLines = map (contextForEachLine (fst cAndLines)) (snd cAndLines) 
		contextForEachLine c ctxAndValues = case (searchDeclByName sig c) of 
			Nothing -> error ("ERROR: adjustByContext" ++ (show sig) ++ c)
			Just (Decl c1 typ info entries) -> 
				let newvars = map (\n -> Var ("E" ++ (show n))) [1 .. (length entries)] in 
				 let n = (fst ctxAndValues) in 
					(
					Rule (toProgressStep n : (map getValue (snd ctxAndValues))) (Formula "step" [] [Constructor c newvars] [Constructor c (replaceAtIndex (n-1) (Var ("E" ++ show n ++ "'")) newvars)]),
					Rule (toContains "contains" n : (map getValue (snd ctxAndValues))) (Formula "contains" [] [Constructor c newvars] [Var "E"]),
					if (elem c1 errorHandlers) || errorHandlers == [] then Nothing else Just (Rule (toContains "containsError" n : (map getValue (snd ctxAndValues))) (Formula "containsError" [] [Constructor c newvars] [Var "E"]))
					)

generate :: IO ()
generate = do
   mapM_ gradualize (tail preLibrary)
   return ()

gradualize :: String -> IO ()
gradualize name = do
               streamSig <- readFile ("Repo of Static Type Systems/" ++ name ++ ".sig") 
               streamMod <- readFile ("Repo of Static Type Systems/" ++ name ++ ".mod") 
               let signature = lines streamSig
               let moduleL = lines streamMod
               let (ts, ctx) = parseLP (signature ++ moduleL) 
               let targetTs = (generateRules ts)
               let targetSig = (generateSigOnlyNew targetTs ts)
               let gradualName = "gradual_" ++ name
               let sigPreamble = "sig " ++ gradualName ++ ".\n\n"
               let modPreamble = "module " ++ gradualName ++ ".\n\n"
               writeFile ("Gradualized/" ++ gradualName ++ ".sig") (sigPreamble ++ (unlines (drop 1 signature)) ++ "\n\n" ++ (toLambdaPrologSig targetSig))
               writeFile ("Gradualized/" ++ gradualName ++ ".mod") (modPreamble ++ (unlines (drop 1 moduleL)) ++ "\n\n" ++ (intercalate "\n" (map toLambdaPrologR (completeRules ts ctx))) ++ "\n\n" ++ (toLambdaPrologModule targetTs))

generateRules :: TypeSystem -> TypeSystem
generateRules (Ts sig rules) = 
	let onlyTypeOfRules = filter (filterRules typeOf) rules in 
	let onlyStepRules = filter (filterRules stepOriginal) rules in 
	let tsOrigin = (Ts sig (onlyTypeOfRules ++ onlyStepRules)) in 
	let ts = (Ts (sig ++ [Decl "dyn" "typ" (Left []) []]) onlyTypeOfRules) in 
	case (addCastManagement tsOrigin (auxiliaryPred ts)) of (Ts sigFinal rulesFinal) -> (Ts sigFinal (filter (not . (filterRules typeOf)) rulesFinal))
-- extend (extend (extend (toTypeSystemForCC ts) (castInsertion ts)) (castManagement (toTypeSystemForCC ts))) (auxiliaryPred (toGradual ts)) 

generateSigOnlyNew :: TypeSystem -> TypeSystem -> Signature
generateSigOnlyNew (Ts sig rules) (Ts sigOriginal _) = nub (sig \\ sigOriginal)
--  (sig_dyntype ++ sig_typeOfGr ++ sig_castCalculus ++ sig_typeOfCI ++ (sigOfNewThings ts) ++ sigOfdynamic ++ stepTicked_ifNecessary ts)

stepTicked_ifNecessary ts = if doesItAppear "step'" ts then [Decl "step'" "o" (Left []) [(Simple "term"), (Simple "term")]] else []

