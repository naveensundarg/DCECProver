## Theory

![alt text](http://www.naveensundarg.com/images/shadow.png "shadow")

## Algorithm Sketch

Every atomic modal formula *m* is assigned a _unique propositional_
   variable *s*: We call *s* the *propositional shadow of m*. For any
   formula *f[m]*, the corresponding formula *f[s]*, with all atomic
   modal formulae replaced by their propositional shadows,  is called
   the shadow of *f[m]*.)
			 
* **Step 1**: The prover first replaces all occurrences of atomic
             modal formulae by propositional variables (even nested
             occurrences).  Now we have a first-order problem.

* **Step 2**:  Call a first-order prover on this first-order problem.

* **Step 3**: If the first-order prover fails, we expand the premises with
 any legal modal rule (via forward reasoning natural deduction) and
 repeat the process from **Step 1**. If the prover succeeds, we return
 true.

