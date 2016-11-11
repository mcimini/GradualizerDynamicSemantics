module DestinationType where

import System.IO.Unsafe 
import Data.Unique
import Data.List
import qualified Data.Map as HM
import TypeSystem

destinationType :: Package -> Term -> Term
destinationType pkg (Var variable) = case flowTermByVar (pkg_getRule pkg) (Var variable) of 
                                        Nothing -> checkIfPatternMatched
                                        Just anotherVar -> anotherVar
                                      where 
                                      checkIfPatternMatched = case patternMatchByVar (pkg_getRule pkg) (Var variable) of
                                        Nothing -> (Var variable)
                                        Just premise -> destinationType pkg (reverseMatch pkg premise) 
destinationType pkg (Constructor c terms) = (Constructor c (map (destinationType pkg) terms)) 
destinationType pkg (Application term1 term2) = (Application (destinationType pkg term1) (destinationType pkg term2)) 
destinationType pkg (Bound variable) = (Bound variable)

flowTermByVar :: Rule -> Term -> Maybe Term
flowTermByVar rule term = case searchPremiseByPredAndVar rule "flow" term of { Nothing -> Nothing ; Just (Formula pred strings interms outterms) -> Just (interms !! 1) }

makeFlow :: Term -> Term -> Premise
makeFlow = \type1 -> \type2 -> Formula "flow" [] [type1, type2] []

-- Given a pattern-maching variable, it searches its pattern-matching premise in the rule
patternMatchByVar :: Rule -> Term -> Maybe Premise
patternMatchByVar rule term = searchPremiseByPredAndVar rule "match" term

-- Given a pattern-matching premise, gives you the entire term. Care is given to put contravariant terms at their place.
reverseMatch :: Package -> Premise -> Term
reverseMatch pkg (Formula pred strings interms outterms) = if pred == "match" then (Constructor (head strings) outterms) else error "reverseMatch called with an argument that is not pattern-matching premise"
