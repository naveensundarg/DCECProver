;;;; ShadowProver.lisp

(in-package #:shadowprover)

(defun declare-sorts ()
  (snark:declare-sort 'Agent)
  (snark:declare-subsort 'Moment 'Number)
  (snark:declare-subsort 'Fluent))


(defun declare-functors ())


(defun true? (x) (prove-from-axioms nil x))
(defun false? (x) (prove-from-axioms nil `(not ,x)))

(defun declare-all-sorts-and-functors ()
  (declare-sorts)
  (declare-functors))

(defun apply-rule (rule &rest args)
  (case rule
    (:and-elim (optima:match (first args) ((list 'and A B) (list A B))))
    (:implies-elim (optima:match (first args) 
                     ((list (list 'implies _ Cons) _) Cons)))
    (:R1 (optima:match args ((list (list 'C _ F)
                                   a1 a2 a3 t1 t2 t3)
                             `(knows ,a1 ,t1 (knows ,a2 ,t2 (knows ,a3 ,t3 ,F))))))
    (:DR6 (optima:match (first args) 
            ((list (list 'knows a time F) _)
             `(knows ,a ,time ,(consequent F)))))
    (otherwise (error "~ unimplemented rule." rule))))

(defun forward (Premises Formula &optional (proof-stack nil))
    (or 
     (handle-DR2 Premises Formula proof-stack)
     (handle-DR3 Premises Formula proof-stack)
     (handle-DR4 Premises Formula proof-stack)
     (handle-DR5 Premises Formula proof-stack)
     (handle-DR6 Premises Formula proof-stack)
     (handle-DR9 Premises Formula proof-stack)
     (handle-R4 Premises Formula proof-stack)
     (handle-and-elim Premises Formula proof-stack)
     (handle-implies-elim Premises Formula proof-stack)
     (introduce-theorems Premises Formula proof-stack)
     (handle-or-elim Premises Formula proof-stack)
     (handle-reductio Premises Formula proof-stack)))

(defun make-declarer  ()
;  (lambda () (declare-all-sorts-and-functors) (mapcar ))
  )

(defun prove (Premises Formula &key (proof-stack nil))
  (if (prove-from-axioms (shadow-all Premises) formula :time-limit 3 :verbose nil) 
       (add-to-proof-stack proof-stack :FOL Formula) 
      (forward Premises Formula proof-stack)))



;;; show code

