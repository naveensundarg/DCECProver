(in-package :shadowprover)



(defmacro define-type-1-expander (name out &rest in) 
  `(defun ,name (premises formula proof-stack)
    (let ((fresh (filter (lambda (formula)
                           (optima:match formula
                           (,(cons 'list in) (if (elem ,out premises) nil t))))
                         (cartesian-power premises ,(length in))))) 
      (if fresh 
          (let ((derived (optima:match (first fresh)
                           (,(cons 'list in) ,out))))
            (prove (cons derived premises)
                   formula
                   :proof-stack 
                   (add-to-proof-stack proof-stack 
                                       ',name 
                                       derived 
                                       (list
                                        (princ-to-string (first fresh))))))))))
