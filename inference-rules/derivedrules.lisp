(in-package #:shadowprover)

(defun unused-DR4-args (premises)
  (filter (lambda (formula)
            (optima:match formula
              ((list 'knows a time F) (if (elem `(believes ,a ,time ,F) premises) nil t))))
          (filter #'knowledge? premises)))


(defun handle-DR4 (Premises Formula proof-stack)
  (let ((unused-DR4-args (unused-DR4-args premises))) 
    (if unused-DR4-args 
        (let ((derived (apply-rule :DR4 (first unused-DR4-args))))
          (prove (cons derived premises)
                 Formula
                 (add-to-proof-stack proof-stack 
                                     :DR4 
                                     derived 
                                     (list
                                      (princ-to-string (first unused-DR4-args)))))))))
