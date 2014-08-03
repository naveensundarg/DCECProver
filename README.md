![alt text](http://www.naveensundarg.com/images/shadow.png "shadow")

* **Step 1**: The prover first replaces all occurrences of atomic modal formulae by propositional variables (even nested occurrences).
             (Each modal formula *m* is assigned a unique
             propositional variable *s*: we call *s* the *propositional
             shadow of m*). Now we have a  first-order problem.

* **Step 2**:  Call a first-order prover on this first-order problem.

* **Step 3**: If the first-order prover fails, we expand the premises a bit
  with some usable modal rule (via forward reasoning natural
  deduction) and repeat the process from  **Step 1**. If the prover
  succeeds, we return true.

