module Library where

import TypeSystem
import ToLambdaProlog

preLibrary :: [String]
preLibrary =
           [
           "none",
           "stlc",
           "stlc_integers",
           "stlc_fix",
           "stlc_if",
           "stlc_inference",
           "stlc_let",
           "stlc_letrec",
           "stlc_lists",
           "stlc_pairs",
           "stlc_sum",
           "stlc_tuples",
           "stlc_unit",
           "stlc_exc",
           "systemF",
           "recursive",
          "stlc_ref",
  --         "stlc_subtype",
           "fpl"
           ]


preLibraryWithTitles  :: [(String, String)]
preLibraryWithTitles =
           [
           ("none", ""),
           ("stlc", "Simply Typed Lambda Calculus (STLC)"),
           ("stlc_add", "STLC with Integers and Addition"),
           ("stlc_exc", "STLC with Exceptions"),
           ("stlc_fix", "STLC with Recursion (Fix operator)"), 
           ("stlc_if", "STLC with Booleans and If-statement"),
           ("stlc_inference", "STLC in Type Inference style"), 
           ("stlc_let", "STLC with Let bindings"),
           ("stlc_letrec", "STLC with Letrec bindings"), 
           ("stlc_lists", "STLC with Lists"), 
           ("stlc_pairs", "STLC with Pairs"), 
           ("stlc_ref", "STLC with References"), 
           ("stlc_subtype", "STLC with Subtyping"),
           ("stlc_sum", "STLC with Sum Types"), 
           ("stlc_tuples", "STLC with the Tuples"), 
           ("stlc_unit", "STLC with the Unit type")
           ]

