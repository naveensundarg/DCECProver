;;;; ShadowProver.lisp

(in-package #:shadowprover)


(defun apply-rule (rule &rest args)
  (case rule
    (:and-elim (optima:match (first args) ((list 'and A B) (list A B))))
    (:implies-elim (optima:match (first args) 
                     ((list (list 'implies _ Cons) _) Cons)))
    (:R1 (optima:match args ((list (list 'C _ F)
                                   a1 a2 a3 t1 t2 t3)
                             `(knows ,a1 ,t1 (knows ,a2 ,t2 (knows ,a3 ,t3 ,F))))))
    (:R4 (optima:match (first args) ((list 'knows _ _ F) F)))
    (:DR4 (optima:match (first args) ((list 'knows a time F) `(believes ,a ,time ,F))))
    (otherwise (error "~ unimplemented rule." rule))))

(defun forward (Premises Formula &optional (proof-stack nil))
    (or 
     (handle-DR4 Premises Formula proof-stack)
     (handle-R4 Premises Formula proof-stack)
     (handle-and-elim Premises Formula proof-stack)
     (handle-implies-elim Premises Formula proof-stack)
     (handle-or-elim Premises Formula proof-stack)))

(defun prove (Premises Formula &optional (proof-stack nil) )
  (if (prove-from-axioms (shadow-all Premises) formula :time-limit 10 :verbose nil) 
       (add-to-proof-stack proof-stack :FOL Formula) 
      (forward Premises Formula proof-stack)))



;;; show code

