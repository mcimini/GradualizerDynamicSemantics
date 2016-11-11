## A mechanized proof that a rich gradually typed language is correct. 

Author: Matteo Cimini (mcimini@indiana.edu)

Language: <br />
<ul>
<li> fpl.mod: A call-by-value functional programming language with integers, booleans, if-then-else, pairs, sums, lists, fix, letrec, polymorphim, recursive types, 
and exceptions.
<li> gradualized in gradual_fpl.mod
</ul>

Quick check for the mechanized proof: <br />
<ul>
<li> make 
 <br />    (* Important: you need the <a href="http://abella-prover.org">Abella proofs assistant</a> installed and "abella" must be in the $PATH *)  
</ul>
Output: <br />
<ul>
<li> Abella machine-checks all theorems and ends with:
<br />
simulation_of_more_precise < search.
<br />
Proof completed.
<br />
<br />
Abella < Goodbye.

<li> The command returns to the prompt. 
<br />(* Important: It may take 2-3 minutes *)  
</ul>
To clean: <br />
<ul>
<li> make clean 
	<br />  (removes compilation files .thc)
</ul>

Current State: Only two lemmas admitted, in less_precise.thm<br />
<ul>
<li> Theorem lessPreciseType_reflexivity.
	<br />  
<li> Theorem less_precise_means_less_precise_type.
</ul>

