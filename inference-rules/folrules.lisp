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


(defun handle-implies-elim (Premises Formula sortal-fn proof-stack)
  (let ((unused-implies-elim-args (unused-implies-elim-args premises))) 
    (if unused-implies-elim-args  
        (prove! 
         (cons 
          (apply-rule :implies-elim (first unused-implies-elim-args))
          premises)
         Formula
         :sortal-fn sortal-fn
         :proof-stack
         (add-to-proof-stack proof-stack :implies-elim 
                              (apply-rule
                               :implies-elim (first unused-implies-elim-args))
                              (first unused-implies-elim-args) )
         :caller (list 'implies-elim :got
                       (apply-rule :implies-elim (first unused-implies-elim-args)))))))


(defun handle-and-elim (Premises Formula sortal-fn proof-stack)
  (let ((unused-and-elim-args (unused-and-elim-args premises))) 
    (if unused-and-elim-args  
        (prove! 
         (append 
          (apply-rule :and-elim (first unused-and-elim-args))
          premises)
         Formula
         :sortal-fn sortal-fn
         :proof-stack 
         (add-to-proof-stack 
          (add-to-proof-stack
           proof-stack
           :and-elim
           (first (apply-rule :and-elim (first unused-and-elim-args)))
           (list (first
                  unused-and-elim-args)))
          :and-elim
          (second (apply-rule :and-elim (first unused-and-elim-args)))
          (list (first
                 unused-and-elim-args)))
         :caller  (list 'and-elim :got (append 
          (apply-rule :and-elim (first unused-and-elim-args))
          premises))))))


(defun handle-or-elim (Premises Formula sortal-fn proof-stack)
  (let ((unused-or-args (unused-or-args premises))) 
    (if unused-or-args 
        (let* ((disjunct (first unused-or-args))
               (disjuncts (args disjunct))
               (left (first disjuncts))
               (right (second disjuncts))
               (reduced-premises (remove disjunct Premises :test #'equalp)) 
               (left-proof 
                (prove! (cons left reduced-premises) 
                        Formula :sortal-fn 
                        sortal-fn :proof-stack proof-stack
                        :caller (list :or-elim :sub-left left )))
               (right-proof 
                (prove! (cons right reduced-premises)
                        Formula :sortal-fn
                        sortal-fn :proof-stack proof-stack
                        :caller (list :or-elim :sub-right right ))))
          (if (and left-proof right-proof)
              (add-to-proof-stack proof-stack :or-elim Formula (list
  (princ-to-string disjunct)) left-proof right-proof))))))

(defparameter *in-reductio?* nil)

(defun handle-reductio (Premises Formula sortal-fn proof-stack)
 ; (format t "In Reductio? ~a~%"  *in-reductio?*)
  (let* ((absurd '(and p  (not p)))
         (meaningful? 
           (not (false? (shadow-formula Formula))))
         (reductio (if (and   meaningful?)
                       (let ((*in-reductio?* t))
                        ; (format t "In Reductio? ~a~%"  *in-reductio?*)
                         (prove! (cons `(not ,Formula) Premises) 
                                 absurd 
                                 :sortal-fn sortal-fn
                                 :proof-stack nil
                                 :caller (list 'from-reductio Formula))))))
    (if reductio (add-to-proof-stack 
                  proof-stack
                  (list 'suppose-absurd
                        `(not ,Formula) 'in
                        (dseq reductio)) formula (premises* reductio)))))



(defun handle-implies-deeper (Premises Formula sortal-fn proof-stack)
  (let ((focus (first (filter #'implies? Premises)))) 
    (if (and
         focus
         (not (elem focus *tackled-implies*)))  
        (progn 
          (setf *tackled-implies* (cons focus *tackled-implies*))
          (if (shadow-prover Premises  (antecedent focus) :sortal-fn sortal-fn)
              (let ((sub-proof (prove! 
                                (cons 
                                 (consequent focus)
                                 premises)
                                Formula
                                :sortal-fn sortal-fn
                                :proof-stack 
                                (add-to-proof-stack proof-stack
                                                    :implies
                                                    focus
                                                    (list (princ-to-string focus))) 
                                :caller (list 'introduce-theorems :got focus))))
                (if sub-proof 
                    sub-proof
                    (progn 
                           (setf *tackled-implies*(remove focus *tackled-implies*))
                           nil)))
              (progn 
                     (setf *tackled-implies*(remove focus *tackled-implies*))
                     nil))))))
