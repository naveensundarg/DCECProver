(in-package #:shadowprover)


(defun unused-DR6-args (premises)
  (filter (lambda (formula)
            (optima:match formula
              ((list (list 'knows a time (list 'implies _ Cons))
                     (list 'knows _ _ _))
               (if (elem `(Knows ,a ,time ,Cons) premises) nil t))))
          (filter (lambda (pair)
                    (let ((K1 (first pair))
                          (K2 (second pair)))
                        (and (knowledge? K1)
                             (knowledge? K2)
                             (implies? (modal-F K1))
                             (equalp (first (args (modal-F K1)))
                                     (modal-F K2)))))
                  (cartesian-product (list premises premises)))))


(defun handle-DR6 (Premises Formula proof-stack)
  (let ((unused-DR6-args (unused-DR6-args premises))) 
    (if unused-DR6-args  
        (prove 
         (cons 
          (apply-rule :DR6 (first unused-DR6-args))
          premises)
         Formula
         :proof-stack
         (cons `(:DR6  ,(list (princ-to-string (first unused-DR6-args))))  
               proof-stack)))))




(define-type-1-expander handle-DR3 P (list 'common _ P))
(define-type-1-expander handle-DR4 (list 'knows a time P) (list 'sees a time P))
(define-type-1-expander handle-DR5 (list 'believes a time P) (list 'knows a time P))
 

;(define-type-1-expander handle-DR4 (list 'knows a time P) (list 'sees _ P))
