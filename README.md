![alt text](http://www.naveensundarg.com/images/shadow.png "shadow")

* Step 1: The prover first replaces all occurences of atomic modal formulae by propositional variables (even nested occurences).
             (Each modal formula is assigned a unique propositional variable: we call it the **propositional shadow**). Now we have a pure FOL problem.

* Step 2:  Call a first-order prover on this pure first-order problem.

* Step 3: If the FOL prover, we expand the premises a bit with some usable modal rule (via forward reasoning natural deduction) and repeat the process from  Step 1. 

