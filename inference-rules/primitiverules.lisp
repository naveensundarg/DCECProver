(in-package #:shadowprover)


(defun unused-R1-args (premises formula)
  (filter (lambda (arg)
            (optima:match arg
              ((list (list 'C _ F) a1 a2 a3 t1 t2 t3)
               (if (elem `(Knows ,a1 ,t1 (Knows ,a2 ,t2 (Knows ,a3 ,t3 ,F)))
                                          premises) nil t))))
          (mapcar (m)
           (filter #'common-knowledge? premises)
           (mapcar (lambda (x))
            (permute (reduce #'append (mapcar #'agents (cons formula premises))))
            (permute (reduce #'append (mapcar #'times (cons formula premises))))))))

(defun unused-R4-args (premises)
  (filter (lambda (formula)
            (optima:match formula
              ((list 'knows _ _ F) (if (elem F premises) nil t))))
          (filter #'knowledge? premises)))


(defun handle-R4 (Premises Formula proof-stack)
  (let ((unused-R4-args (unused-R4-args premises))) 
    (if unused-R4-args 
        (let ((derived (apply-rule :R4 (first unused-R4-args))))
          (prove (cons derived premises)
                 Formula
                 (add-to-proof-stack proof-stack 
                                     :R4 
                                     derived 
                                     (list
                                      (princ-to-string (first unused-R4-args)))))))))











