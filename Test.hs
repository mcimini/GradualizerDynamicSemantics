module Test where


import           System.IO
import           System.Process
import           System.Directory
import           Control.Monad
import           TypeSystem
import           GenerateCalculi
import           FromLambdaProlog
import           ToLambdaProlog
import           Library
--import           ToLatex

test :: IO ()
test = do
   mapM_ unitTest (tail preLibrary)
   mapM_ typeability (tail preLibrary)
   return ()

testNoTypeCheck :: IO ()
testNoTypeCheck = do
   mapM_ unitTest (tail preLibrary)
   return ()
   
unitTest :: String -> IO ()
unitTest systemName = do
   gradualize systemName

typeability :: String -> IO ()
typeability systemName = do
    old <- getCurrentDirectory
    setCurrentDirectory "Gradualized/"
    mapM_ unitTypeTest (tail preLibrary)
    setCurrentDirectory old

unitTypeTest :: String -> IO ()
unitTypeTest systemName = do
    let source = "gradual_" ++ systemName
    callCommand ("tjcc " ++ source ++ " > " ++ systemName ++ ".log")

parseAndShowAll :: IO [()]
parseAndShowAll = do
       mapM parseAndShow (tail preLibrary)
       
parseAndShow :: String -> IO ()
parseAndShow name = do
               streamSig <- readFile ("Repo of Static Type Systems/" ++ name ++ ".sig") 
               streamMod <- readFile ("Repo of Static Type Systems/" ++ name ++ ".mod")
               let signature = lines streamSig
               let moduleL = lines streamMod
               let (ts, _) = parseLP (signature ++ moduleL)
               putStrLn (toLambdaPrologModule ts)
			   

parseAndSpitTS :: String -> IO TypeSystem
parseAndSpitTS name = do
               streamSig <- readFile ("Repo of Static Type Systems/" ++ name ++ ".sig") 
               streamMod <- readFile ("Repo of Static Type Systems/" ++ name ++ ".mod")
               let signature = lines streamSig
               let moduleL = lines streamMod
               let (ts, _) = parseLP (signature ++ moduleL)
               return ts
