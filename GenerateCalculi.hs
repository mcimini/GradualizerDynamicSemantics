module GenerateCalculi where

import System.IO
import System.IO.Unsafe
import Data.Unique
import Data.List
import TypeSystem
--import PatternMatching
--import FlowDiscovery
--import Static
import CastInsertion
import GenerateAuxiliaryPredicates
import FromLambdaProlog
import CastManagement
import ToLambdaProlog
import Library

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
               let (ts, contextRules) = parseLP (signature ++ moduleL)
               let targetTs = (generateRules ts)
               let targetSig = (generateSigOnlyNew targetTs ts)
               let gradualName = "gradual_" ++ name
               let sigPreamble = "sig " ++ gradualName ++ ".\n\n"
               let modPreamble = "module " ++ gradualName ++ ".\n\n"
               writeFile ("Gradualized/" ++ gradualName ++ ".sig") (sigPreamble ++ (unlines (drop 1 signature)) ++ "\n\n" ++ (toLambdaPrologSig targetSig))
               writeFile ("Gradualized/" ++ gradualName ++ ".mod") (modPreamble ++ (unlines (drop 1 moduleL)) ++ "\n\n" ++ (intercalate "\n" (map toLambdaPrologR contextRules)) ++ "\n\n" ++ (toLambdaPrologModule targetTs))

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

