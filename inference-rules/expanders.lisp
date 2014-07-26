(in-package :shadowprover)



(defmacro define-type-1-expander (name constraint out &rest in)  
  `(defun ,name (premises formula sortal-fn proof-stack)
    (let ((fresh (filter 
                  (lambda (formula)
                    (optima:match formula
                      (,(cons 'list in) (if (elem ,out premises) nil t))))
                  (let ((premises^n (cartesian-power premises ,(length in))))
                    (if ,constraint (filter (lambda (premises-tuple) 
                                              (apply ,constraint premises-tuple)) premises^n) 
                        premises^n))))) 
      (if fresh 
          (let ((derived (optima:match (first fresh)
                           (,(cons 'list in) ,out))))
            (prove! (cons derived premises)
                   formula
                   :sortal-fn sortal-fn
                   :proof-stack 
                   (add-to-proof-stack proof-stack 
                                       ',name 
                                       derived 
                                       (list
                                        (princ-to-string (first fresh)))) 
                   :caller ',name))))))
