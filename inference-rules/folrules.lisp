(in-package #:shadowprover)


(defun unused-or-args (premises)
  (filter #'or? premises))

(defun unused-and-elim-args (premises)
  (filter (lambda (formula)
            (optima:match formula
              ((list 'and A B) (if (and (elem A premises)
                                        (elem B premises)) nil t))))
          (filter #'and? premises)))


(defun unused-implies-elim-args (premises)
  (filter (lambda (formula)
            (optima:match formula
              ((list (list 'implies _ Cons) _) (if (elem Cons premises) nil t))))
          (filter (lambda (pair)
                    (and (implies? (first pair))
                         (equalp (first (args (first pair)))
                                 (second pair))))
                  (cartesian-product (list premises premises)))))


(defun handle-implies-elim (Premises Formula proof-stack)
  (let ((unused-implies-elim-args (unused-implies-elim-args premises))) 
    (if unused-implies-elim-args  
        (prove 
         (cons 
          (apply-rule :implies-elim (first unused-implies-elim-args))
          premises)
         Formula
         (cons `(:implies-elim  ,(list (princ-to-string (first unused-implies-elim-args))))  
               proof-stack)))))


(defun handle-and-elim (Premises Formula proof-stack)
  (let ((unused-and-elim-args (unused-and-elim-args premises))) 
    (if unused-and-elim-args  
        (prove 
         (append 
          (apply-rule :and-elim (first unused-and-elim-args))
          premises)
         Formula
         (add-to-proof-stack proof-stack
                             :and-elim
                             (rest (first unused-and-elim-args))
                             (list (princ-to-string (first unused-and-elim-args))))))))


(defun handle-or-elim (Premises Formula proof-stack)
  (let ((unused-or-args (unused-or-args premises))) 
    (if unused-or-args 
        (let* ((disjunct (first unused-or-args))
               (disjuncts (args disjunct))
               (left (first disjuncts))
               (right (second disjuncts))
               (reduced-premises (remove disjunct Premises :test #'equalp)) 
               (left-proof 
                (prove (cons left reduced-premises) Formula proof-stack))
               (right-proof 
                (prove (cons right reduced-premises) Formula  proof-stack)))
          (if (and left-proof right-proof)
              (add-to-proof-stack proof-stack :or-elim Formula (list  (princ-to-string disjunct)) left-proof right-proof))))))
