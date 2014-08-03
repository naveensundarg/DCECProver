

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Parameters ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *tackled-implies* nil)
(defparameter *fol-counts* 0)
(defparameter *modal-counts* 0)
(defparameter *interactive* t)
(defparameter *line-number* 1)
(defparameter *snark-verbose* nil)
(defparameter *expanded* nil)

(defparameter *premises* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Proof Calculus ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *proof-calculus*
  (handle-DR2 
   handle-DR3
   handle-DR4
   handle-DR5
   handle-DR6
   handle-DR12
   handle-R4
   handle-and-elim
   handle-implies-elim
   handle-implies-deeper
   handle-or-elim
   handle-univ-elim
   handle-reductio
   handle-DR1
   handle-DR9
   handle-DR19)  )


 
