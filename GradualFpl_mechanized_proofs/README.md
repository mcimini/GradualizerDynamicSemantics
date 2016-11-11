## A Gradualizer for the Dynamic Semantics of Gradually Typed Languages

Author: Matteo Cimini (mcimini@indiana.edu)

Quick usage: <br />
<ul>
<li> make 
 <br />    (* Important: you need the <a href="http://abella-prover.org">Abella proofs assistant</a> installed and "abella" must be in the $PATH *)  
</ul>
 <br />
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
<br />
To clean: <br />
<ul>
<li> make clean 
	<br />  (removes compilation files .thc)
</ul>

