module FromLambdaProlog (parseLP) where

import Prelude
import Data.String.Utils
import Data.Maybe
import TypeSystem
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

type LogicalLine = (String, (Int, Int))
type ContextInfo = Maybe [(Int,[Int])]

contextTag = "% context"
errorHandlerTag = "% errorHandler"
contraTag = "% variance"
elimTag = "% eliminator"
invLabel = "INV"
covLabel = "COV"
contraLabel = "CONTRA"
myTags = [contextTag, errorHandlerTag, contraTag, elimTag, invLabel, covLabel, contraLabel]
startsWithMyTags line = any (\tag -> startswith tag line) myTags

defaultInfo :: [TypeEntry] -> Either VarianceInfo Int
defaultInfo entries = case (toStringTE (last entries)) of
	"typ" -> (Left (replicate ((length entries)-1) Cov))
	_ -> (Right 0)
	

-- It stores number of inputs, at the beginning, per predicate. This below means typeOf is inputs/output while subtype is input/input
modeTable :: [(String, Int)]
modeTable = [ 	("typeOf", 1),
				("subtype", 2)
			]
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where style = emptyDef {
          Token.reservedNames = ["(pi x\\", "=> typeOf",":-",".","(",")","x","X","type","->", "[", "]"] ++ myTags}

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

identifier :: Parser String
identifier = Token.identifier lexer

conId :: Parser String
conId = do { c <- lower
           ; cs <- many alphaNum
           ; return $ (c:cs) }

number :: Parser Int
number = do { cs <- many1 digit
            ; return $ read cs }
numberBracket :: Parser Int
numberBracket = do { cs <- many1 digit
			  ; _ <- char '['
            ; return $ read cs }

-- Parser
{-
variable :: Parser Term
variable = Var `fmap` identifier
-}

-- (map (adjustByContra contraEntries) sig) is the adjusted sig.
tsParse :: Parser (TypeSystem, [Rule])
tsParse = do { sig <- many signature
	     		; rules <- many ruleGram
                ; contexts <- many contextTags
                ; elimEntries <- many elimTags
                ; errorHandler <- many errorHandlerTags
                ; contraEntries <- many contravarTags
             ; let newsig = sig_insertElimininators elimEntries (sig_insertVariance contraEntries sig)  
--	     ; return $ (Ts newsig (rules ++ (completeRules (Ts newsig rules) contexts))) }
	     ; return $ (Ts newsig rules, (completeRules (Ts newsig rules) contexts errorHandler)) }
        
contextTags :: Parser (String, [(Int, [Int])])
contextTags = do {
        ; reserved contextTag
        ; c <- Token.lexeme lexer conId
        ; positions <- Token.commaSep1 lexer ctx_position
  		; reserved "."
        ; return (c, positions) }

errorHandlerTags :: Parser (String)
errorHandlerTags = do {
        ; reserved errorHandlerTag
        ; c <- Token.lexeme lexer conId
  		; reserved "."
        ; return c }

ctx_position :: Parser (Int, [Int])
ctx_position = do {
        ; pos <- numberBracket -- Token.lexeme lexer number
--        ; reserved "["
        ; values <- Token.commaSep lexer number
		; reserved "]"
		; return (pos, values) }

contravarTags :: Parser (String, VarianceInfo)
contravarTags = do {
        ; reserved contraTag
        ; c <- Token.lexeme lexer conId
        ; labels <- many contravarLabel
		; reserved "."
        ; return (c, labels) }

contravarLabel :: Parser VarianceLabel
contravarLabel = 
     (do { reserved invLabel ; return Inv }) <|> 
	 (do { reserved covLabel ; return Cov }) <|> 
	 (do { reserved contraLabel ; return Contra })

elimTags :: Parser (String, ElimInfo)
elimTags = do {
        ; reserved elimTag
        ; c <- Token.lexeme lexer conId
        ; n <- Token.lexeme lexer number -- eliminating argument
		; reserved "."
        ; return (c, n) }

ruleP :: Parser Rule
ruleP = do
	myformula <- formula
  	reserved ":-"
	prems <- Token.commaSep1 lexer premise
	reserved "."
	return (Rule prems myformula)


fact :: Parser Rule
fact = do
	frm <- formula 
  	reserved "."
	return (Rule [] frm)

ruleGram :: Parser Rule
ruleGram = try fact <|> ruleP 

formula :: Parser Premise
formula = do
	pred <- Token.lexeme lexer conId
	tterms <- many term 
        case lookup pred modeTable of { Nothing -> return (Formula pred [] tterms []) ; Just numero -> return (Formula pred [] (take numero tterms) (drop numero tterms))}

hypothetical :: Parser Premise
hypothetical = do
	reserved "(pi x\\"
--	reserved "("
	formula1 <- (formula)
	reserved "=>"
	formula2 <- (formula)
--	reserved ")"
	reserved ")"
	return (Hypothetical formula1 formula2)

generic :: Parser Premise
generic = do
	reserved "(pi x\\"
	mypremise <- (premise) -- notice: premise, not formula
	reserved ")"
	return (Generic mypremise)

premise :: Parser Premise
premise = try (generic) <|> (formula) <|> (hypothetical) -- <|> parens (formula pkg)

term ::  Parser Term
--term pkg = try variable <|> boundVar <|> (constructor pkg) <|> application -- <|> (parens (term pkg)) 
term = try (parens (applicationBin)) <|> try (parens (applicationTri)) <|> boundVar <|> variable <|> (parens (constructor)) 

constructor :: Parser Term
constructor = do { 	
--	reserved "(" 
--	; 
	c <- Token.lexeme lexer conId
	; terms <- many (term)
-- 	; reserved ")" 
	; return (Constructor c terms) }

applicationBin :: Parser Term
applicationBin = do {
--	reserved "(" 
--	; 
	var <- variable
	; myterm <- (term)--(boundVar <|> variable)
-- 	; reserved ")" 
	; return (Application var myterm)}

applicationTri :: Parser Term
applicationTri = do {
--	reserved "(" 
--	; 
	var <- variable
	; term1 <- (term)--(boundVar <|> variable)
	; term2 <- (term)--(boundVar <|> variable)
-- 	; reserved ")" 
	; return (Application (Application var term1) term2)}

boundVar :: Parser Term
boundVar = do { reserved "x" ; return (Bound "x")} <|> do { reserved "X" ; return (Bound "X")}

variable :: Parser Term
variable = do 
	name <- Token.lexeme lexer varId
	return (Var name)

varId :: Parser String
varId = do { c <- upper
           ; cs <- many (alphaNum <|> char '\'')
           ; return (c:cs) }

{-
withParens :: Parser Term
withParens = do
	reserved "("
	tterm <- term
	reserved ")"
	return tterm
-}
	
allOf :: Parser a -> Parser a
allOf p = do
  (Token.whiteSpace lexer)
  r <- p
  eof
  return r

parseLP :: [String] -> (TypeSystem, [Rule])
parseLP t = let stuffToParse = unlines (filter (not . parseGarbage) t) in 
  case parse (allOf tsParse) "stdin" stuffToParse of
    Left err -> error (show err)
    Right ast -> ast

parseGarbage :: String -> Bool
parseGarbage line = 
	startswith "sig" line 
    || startswith "kind" line 
	|| startswith "module" line 
	|| startswith "\n" line 
	|| (startswith "%" line && (not (startsWithMyTags line)))

signature :: Parser SignatureEntry
signature = do
        reserved "type"
        c <- Token.lexeme lexer conId
        entries <- sepBy typeEntry (do {Token.whiteSpace lexer; string "->"; Token.whiteSpace lexer})
  	reserved "."
        return (Decl c (toStringTE (last entries)) (defaultInfo entries) (init entries))

typeEntry :: Parser TypeEntry
typeEntry = try simple <|> parens hoas 

simple :: Parser TypeEntry
simple = do
        item <- Token.lexeme lexer conId
        return (Simple item)

hoas :: Parser TypeEntry
hoas = do
        items <- sepBy (Token.lexeme lexer conId) (do {Token.whiteSpace lexer; string "->"; Token.whiteSpace lexer})
        if (length items) == 2 then return (Abs (items !! 0) (items !! 1)) else error "Parsing Error: At the moment, The use of HOAS is restriced to only abstractions of the form (type1 -> type2) (i.e. only one abstracted argument) "

sig_insertVariance :: [(String, VarianceInfo)] -> Signature -> Signature
sig_insertVariance [] sig = sig
sig_insertVariance (entry:rest) sig = 
 case entry of (c, varianceLabels) -> case (searchDeclByName sig c) of { Nothing -> error ("ERROR: sig_insertVariance. not found: " ++ (show sig) ++ c) ; Just (Decl c1 typ info entries) -> (Decl c1 typ (Left varianceLabels) entries):(sig_insertVariance rest (deleteDeclByName sig c)) }

sig_insertElimininators :: [(String, ElimInfo)] -> Signature -> Signature
sig_insertElimininators [] sig = sig
sig_insertElimininators (entry:rest) sig = 
 case entry of (c, n) -> case (searchDeclByName sig c) of { Nothing -> error ("ERROR: sig_insertElimininators. not found: " ++ (show sig) ++ c) ; Just (Decl c1 typ info entries) -> (Decl c1 typ (Right n) entries):(sig_insertElimininators rest (deleteDeclByName sig c)) }

completeRules :: TypeSystem -> [(String, [(Int, [Int])])] -> [String] -> [Rule]
completeRules (Ts sig rules) contexts errorHandlers = (map fstOf3 tripleOfRulesContextAndContainsAndError) 
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

getRidOfNone :: [Maybe Rule] -> [Rule]
getRidOfNone maybelist = map fromJust $ filter isJust maybelist