(in-package :shadowprover)

 (declaim (ftype function prove!))
 (declaim (ftype function add-to-proof-stack))

;; Do backward reasoning only if we are able to get a significantly smaller formula.
(defun backward-DR1 (Premises Formula sortal-fn &optional (proof-stack nil))
  (optima:match Formula
    ((list 'knows _ t1 (list 'knows _ t2 P))
     (if (equalp t1 t2) 
         (prove! Premises `(Common ,t1 ,P) 
                 :sortal-fn sortal-fn
                 :proof-stack (add-to-proof-stack proof-stack
                                                  :DR1 Formula Premises)
                  :caller (list :backwards-dr1 Formula))))))


(defun backward-DR2 (Premises Formula sortal-fn &optional (proof-stack nil))
  (optima:match Formula
    ((list 'knows _ time P)
     (if (elem `(Common ,time ,P) Premises)
         (add-to-proof-stack proof-stack
                             :DR1 Formula Premises)))))
