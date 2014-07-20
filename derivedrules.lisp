(in-package #:shadowprover)



(defun handle-DR4 (Premises Formula proof-stack)
  (let ((unused-DR4-args (unused-DR4-args premises))) 
    (if unused-R4-args 
        (let ((derived (apply-rule :DR4 (first unused-DR4-args))))
          (prove (cons derived premises)
                 Formula
                 (add-to-proof-stack proof-stack 
                                     :R4 
                                     derived 
                                     (list
                                      (princ-to-string (first unused-R4-args)))))))))
