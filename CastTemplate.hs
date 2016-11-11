module CastTemplate where

import Data.String.Utils
import Data.Char
import Data.List
import TypeSystem
--import Static
import CastInsertion

--withNegation = False
withNegation = True

sig_dyntype :: [SignatureEntry]
sig_dyntype = [Decl "dyn" "typ" (Left []) []]

listOfTypesMinusDyn :: Signature -> Signature
listOfTypesMinusDyn sig = delete (head sig_dyntype) (listOfTypes sig)

dyntype :: Term
dyntype = (Constructor "dyn" [])

blame :: Term -> Term -> Term
blame typ label = (Constructor "blame" [typ, label])

sigCastCalculus :: [SignatureEntry]
sigCastCalculus = [
					Decl "ground" "o" (Left []) [Simple "typ"],
                    Decl "cast" "term" (Right 0) [(Simple "term"), (Simple "typ"), (Simple "label"), (Simple "typ")],
                    Decl "blame" "term" (Right 0) [(Simple "typ"), (Simple "label")],
					Decl typeOfCC "o" (Left []) [Simple "term", Simple "typ"],
					Decl step "o" (Left []) [Simple "term", Simple "term"],
					Decl "getGroundOf" "o" (Left []) [Simple "typ", Simple "typ"]
                    ]
sigAuxiliaryProlog :: Signature
sigAuxiliaryProlog = [
                  Decl "sameGround" "o" (Left []) [Simple "typ", Simple "typ"]
--                Decl "error" "o" (Left []) [Simple "term"]
--                Decl "equal" "o" Nothing [Simple "term", Simple "term"]
               ]

sigAuxiliaryAbella :: Signature
sigAuxiliaryAbella = [
                Decl "different" "o" (Left []) [Simple "typ", Simple "typ"],
                Decl "different_hi" "o" (Left []) [Simple "typ", Simple "typ"]
--                Decl "error" "o" (Left []) [Simple "term"]
--                Decl "equal" "o" Nothing [Simple "term", Simple "term"]
               ]

getTemplate :: TypeSystem -> TypeSystem
getTemplate (Ts sig rules) = 
	let castManagement_rules = if withNegation then (map getGroundOf_rules (listOfTypesMinusDyn sig)) ++ sameGround_rules ++ castManagement_rulesForProlog else castManagement_rulesForAbella ++ (map getGroundOf_rules (listOfTypesMinusDyn sig)) ++ (auxiliaryDefinitionsPos sig) in 
	let sigAuxiliary = if withNegation then sigAuxiliaryProlog else sigAuxiliaryAbella in 
	(Ts 
		(sigCastCalculus ++ sigAuxiliary)
		(groundDefinitions sig ++ valuesDefinitions sig ++ typeSystemCC rules ++ castManagement_rules )
	)
	
	
typeSystemCC :: [Rule] -> [Rule]	
typeSystemCC rules = map (renamingInRule typeOf typeOfCC) (filter (filterRules typeOf) rules) 
	++ 
	[(Rule
	                [(Formula typeOfCC [] [(Var "E")] [(Var "T1")]), (Formula consistency [] [(Var "T1")] [(Var "T2")])]
	                (Formula typeOfCC [] [(Constructor "cast" [Var "E", Var "T1", Var "L", Var "T2"])] [(Var "T2")])),
	         (Rule
	                []
	                (Formula typeOfCC [] [blame (Var "T") (Var "L")] [(Var "T")])
	          )]
	

valuesDefinitions :: Signature -> [Rule]
valuesDefinitions sig = value_groundCast ++ (concat (map value_constrCast (listOfTypesMinusDyn sig)))

value_groundCast :: [Rule]
value_groundCast = [(Rule
                        [(Formula "value" [] [(Var "V")] []),
                        (Formula "ground" [] [(Var "G")] [])]
                        (Formula "value" [] [(Constructor "cast" [Var "V", Var "G", Var "L", dyntype])] [])
                  )]

value_constrCast :: SignatureEntry -> [Rule]
value_constrCast (Decl c typ info entries) = if entries == [] then [] else 
                 let newVarsT = map (\n -> Var ("T" ++ (show n))) [1 .. (length (entries))] in
                 let newVarsTticked = map (enc "") newVarsT in 
                 [(Rule
                        [(Formula "value" [] [(Var "V")] [])]
                        (Formula "value" [] [(Constructor "cast" [Var "V", Constructor c newVarsT, Var "L", Constructor c newVarsTticked])] [])
                  )]

groundDefinitions :: Signature -> [Rule]
groundDefinitions sig = (map ground_def (listOfTypesMinusDyn sig)) 

ground_def :: SignatureEntry -> Rule
ground_def (Decl c typ info entries) = (Rule [] (Formula "ground" [] [(Constructor c (mapi groundFor entries))] []))
	where
		groundFor entry i = case entry of 
							  (Simple "typ") -> dyntype
							  (Abs var ee)  -> (Lambda "x" dyntype) 
							  otherwise  -> (Var ("X" ++ show i))

--auxiliaryDefinitionsNeg :: Signature -> [Rule]
--auxiliaryDefinitionsNeg sig = (map getGroundOf_rules (listOfTypesMinusDyn sig)) ++ sameGround_rules

castManagement_rulesForProlog :: [Rule]
castManagement_rulesForProlog = [
						(Rule
                        		[Formula "contains" [] [Var "E1", Var "E"] []]
                        		(Formula "contains" [] [Constructor "cast" [Var "E1", Var "T1", Var "L", Var "T2"], Var "E"] [])
                  		),
						(Rule
                        		[Formula "containsError" [] [Var "E1", Var "E"] []]
                        		(Formula "containsError" [] [Constructor "cast" [Var "E1", Var "T1", Var "L", Var "T2"], Var "E"] [])
                  		),
						(Rule
                        		[(Formula "step" [] [(Var "E")] [(Var "E'")])]
                        		(Formula "step" [] [(Constructor "cast" [Var "E", Var "T1", Var "L", Var "T2"])] [(Constructor "cast" [Var "E'", Var "T1", Var "L", Var "T2"])])
                  		),
                        (Rule
                                [(Formula "value" [] [(Var "V")] [])]
                                (Formula step [] [(Constructor "cast" [Var "V", Var "T", Var "L", Var "T"])] [Var "V"])
                        ),
                        let nestedCast = (Constructor "cast" [Var "V", Var "G", Var "L1", dyntype]) in 
                        (Rule
                                [
                                (Formula "value" [] [(Var "V")] []),
                                (Formula "ground" [] [(Var "G")] [])]
                                (Formula step [] [(Constructor "cast" [nestedCast, dyntype, Var "L2", Var "G"])] [Var "V"])
                        ),
                        let nestedCast = (Constructor "cast" [Var "V", Var "G1", Var "L1", dyntype]) in 
						(Rule
                                [
                                (Formula "value" [] [(Var "V")] []),
                                (Formula "ground" [] [(Var "G1")] []),
                                (Formula "ground" [] [(Var "G2")] []),
                                (Negated (Formula "sameGround" [] [(Var "G1"), (Var "G2")] []))
                                ]
                                (Formula step [] [(Constructor "cast" [nestedCast, dyntype, Var "L2", Var "G2"])] [blame (Var "G2") (Var "L2")])
                        ),
                        (Rule
                                [(Formula "value" [] [(Var "V")] []),
                                (Formula "getGroundOf" [] [(Var "T")] [(Var "G")]),
                                (Negated (Formula "ground" [] [(Var "T")] []))
                                ]
                                (Formula step [] [(Constructor "cast" [Var "V", Var "T", Var "L", dyntype])]
                                                [(Constructor "cast" [(Constructor "cast" [Var "V", Var "T", Var "L", (Var "G")]), Var "G", Var "L", dyntype])])
                        ),
                        (Rule
                                [(Formula "value" [] [(Var "V")] []),
                                (Formula "getGroundOf" [] [(Var "T")] [(Var "G")]),
                                (Negated (Formula "ground" [] [(Var "T")] []))
                                ]
                                (Formula step [] [(Constructor "cast" [Var "V", dyntype, Var "L", Var "T"])]
                                                [(Constructor "cast" [(Constructor "cast" [Var "V", dyntype, Var "L", (Var "G")]), Var "G", Var "L",  Var "T"])])
                        ),
						(Rule
                                [
								(Formula typeOfCC [] [(Var "E")] [(Var "T1")]),
								(Formula "contains" [] [(Var "E"), blame (Var "T2") (Var "L")] []),
								 Negated (Formula "=" [] [(Var "E"), blame (Var "T2") (Var "L")] [])
                                ]
                                (Formula "stepC" [] [(Var "E")] [blame (Var "T1") (Var "L")])
						)

{-                        (Rule
                                [
								(Formula "contains" [] [(Var "E"), blame (Var "L")] []),
								(Negated (Formula "=" [] [(Var "E"), blame (Var "L")] []))
                                ]
                                (Formula step [] [(Var "E")] [blame (Var "L")])
						)
-}
						]

getGroundOf_rules :: SignatureEntry -> Rule
getGroundOf_rules (Decl c typ info entries) =
                 let newVarsX = map (\n -> Var ("X" ++ (show n))) [1 .. (length (entries))] in 
				  let groundFor entry i = (case entry of 
					  (Simple "typ") -> dyntype
					  (Abs var ee)  -> (Lambda "x" dyntype) 
					  otherwise  -> newVarsX !! i)
					   in 
					    (Rule []
                               (Formula "getGroundOf" [] [(Constructor c newVarsX)] [(Constructor c (mapi groundFor entries))])
                        )

sameGround_rules :: [Rule]
sameGround_rules = [
                        (Rule
                                [(Formula "getGroundOf" [] [(Var "T1")] [(Var "X")]), (Formula "getGroundOf" [] [(Var "T2")] [(Var "X")])]
                                (Formula "sameGround" [] [(Var "T1"), (Var "T2")] [])
                        )]
						
						
castManagement_rulesForAbella :: [Rule]
castManagement_rulesForAbella = [
						(Rule
                        		[Formula "contains" [] [Var "E1", Var "E"] []]
                        		(Formula "contains" [] [Constructor "cast" [Var "E1", Var "T1", Var "L", Var "T2"], Var "E"] [])
                  		),
                        (Rule
                                [(Formula "value" [] [(Var "V")] [])]
                                (Formula step [] [(Constructor "cast" [Var "V", Var "T", Var "L", Var "T"])] [Var "V"])
                        ),
                        let nestedCast = (Constructor "cast" [Var "V", Var "G", Var "L1", dyntype]) in 
                        (Rule
                                [
                                (Formula "value" [] [(Var "V")] []),
                                (Formula "ground" [] [(Var "G")] [])]
                                (Formula step [] [(Constructor "cast" [nestedCast, dyntype, Var "L2", Var "G"])] [Var "V"])
                        ),
                        let nestedCast = (Constructor "cast" [Var "V", Var "G1", Var "L1", dyntype]) in 
						(Rule
                                [
                                (Formula "value" [] [(Var "V")] []),
                                (Formula "ground" [] [(Var "G1")] []),
                                (Formula "ground" [] [(Var "G2")] []),
                                (Formula "different" [] [(Var "G1"), (Var "G2")] [])
                                ]
                                (Formula step [] [(Constructor "cast" [nestedCast, dyntype, Var "L2", Var "G2"])] [blame (Var "G2") (Var "L2")])
                        ),
                        (Rule
                                [(Formula "value" [] [(Var "V")] []),
                                (Formula "getGroundOf" [] [(Var "T")] [(Var "G")]),
                                (Formula "different_hi" [] [(Var "T"), (Var "G")] [])
                                ]
                                (Formula step [] [(Constructor "cast" [Var "V", Var "T", Var "L", dyntype])]
                                                [(Constructor "cast" [(Constructor "cast" [Var "V", Var "T", Var "L", (Var "G")]), Var "G", Var "L", dyntype])])
                        ),
                        (Rule
                                [(Formula "value" [] [(Var "V")] []),
                                (Formula "getGroundOf" [] [(Var "T")] [(Var "G")]),
                                (Formula "different_hi" [] [(Var "T"), (Var "G")] [])
                                ]
                                (Formula step [] [(Constructor "cast" [Var "V", dyntype, Var "L", Var "T"])]
                                                [(Constructor "cast" [(Constructor "cast" [Var "V", dyntype, Var "L", (Var "G")]), Var "G", Var "L",  Var "T"])])
                        )
{-                        (Rule
                                [
								(Formula "contains" [] [(Var "E"), blame (Var "L")] []),
								(Negated (Formula "=" [] [(Var "E"), blame (Var "L")] []))
                                ]
                                (Formula step [] [(Var "E")] [blame (Var "L")])
						)
-}
						]


different_hi_inductive :: SignatureEntry -> Rule
different_hi_inductive (Decl c typ info entries) = 
	let newVarsT = map (\n -> Var ("T" ++ (show n))) [1 .. (length (entries))] in 
     let newVarsTticked = map (enc "") newVarsT in 	
	  let diverse (t1, t2) i = (case entries !! i of 
		   (Simple "typ") -> (Formula "different" [] [t1,t2] [])
		   (Abs var ee) -> Generic (Formula "different" [] [Application t1 (Bound "X"), Application t2 (Bound "X")] []))
		   in 
		   (Rule (mapi diverse (zip newVarsT newVarsTticked)) (Formula "different_hi" [] [(Constructor c newVarsT), (Constructor c newVarsTticked)] []))

auxiliaryDefinitionsPos :: Signature -> [Rule]
auxiliaryDefinitionsPos sig = 
	[(Rule
            [
            (Formula "different" [] [(Var "T1"), (Var "T2")] [])
			]
            (Formula "different_hi" [] [(Var "T1"), (Var "T2")] [])
    )]
	++  
	(map different_hi_inductive (filter (not . (\decl -> case decl of Decl c1 typ info entries -> entries == [])) (listOfTypes sig))) 
	++ 
	concat (map (different_combinatorics (listOfTypes sig)) (listOfTypes sig)) 

different_combinatorics :: Signature -> SignatureEntry -> [Rule]
different_combinatorics types (Decl c typ info entries) = 
	let newVarsT = map (\n -> Var ("T" ++ (show n))) [1 .. (length (entries))] in 
	 concat (map (different_combinatorics_one (Constructor c newVarsT)) types)

different_combinatorics_one (Constructor c1 newVarsT) (Decl c2 typ info entries) = 
	let newVarsTSecond = map (\n -> Var ("T" ++ (show n))) [1 .. (length (entries))] in 
     let newVarsTticked = map (enc "") newVarsTSecond in 	
	  if c1 == c2 then [] else [(Rule [] (Formula "different" [] [(Constructor c1 newVarsT), (Constructor c2 newVarsTticked)] []))]



--

{-(Decl c typ info entries) =
                 let newVarsX = map (\n -> Var ("X" ++ (show n))) [1 .. (length (entries))] in 
				  let groundFor entry i = (case entry of 
					  (Simple "typ") -> dyntype
					  (Abs var ee)  -> (Lambda "x" dyntype) 
					  otherwise  -> newVarsX !! i)
					   in 
					    (Rule []
                               (Formula "getGroundOf" [] [(Constructor c newVarsX)] [(Constructor c (mapi groundFor entries))])
                        )
-}