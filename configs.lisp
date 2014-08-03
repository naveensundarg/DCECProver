(in-package #:shadowprover)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Parameters ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *interactive* nil)
(defparameter *snark-verbose* t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Proof Calculus ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *forward-proof-calculus*
  '(handle-DR2 
    handle-DR3
    handle-DR4
    handle-DR5
    handle-DR6
    handle-DR12
    handle-R4
    handle-R13
    handle-and-elim
    handle-implies-elim
    handle-implies-deeper
    handle-or-elim
    handle-univ-elim
    handle-reductio
    handle-DR1
    handle-DR9
    handle-DR19))

(defparameter *backward-proof-calculus*
  '())


 
