;;;; ShadowProver.lisp

(in-package #:shadowprover)
(defun elem (e set) (member e set :test #'equalp))
(defun args (F) (rest F))
(defun cartesian-product (sets)
  (if ( < (length sets) 2) 
      (mapcar (lambda (x) (list x)) (first sets))
      (let ((rst (cartesian-product (rest sets))))
        (apply #'append 
               (mapcar (lambda (x) (mapcar (lambda (y) (cons x y)) rst)) 
                       (first sets))))))
(defun permute (seq)
  (cond ((null seq) '())
        ((= 1 (length seq)) (list seq))
        (t (reduce #'append (mapcar (lambda (element) 
		     (mapcar (lambda (l) (cons element l))
                             (permute (remove element seq))))
		   seq)))))

(defun add-to-proof-stack (proof-stack rule out &rest args)
  (list (princ-to-string out) (append (list rule proof-stack) args)))
(defun is-modal? (F)
  (optima:match F 
    ((or
      (list 'common _ _)
      (list 'knows _ _ _) 
      (list 'believes _ _ _)) t)
    (_ nil)))

(defun shadow-formula (formula)
  "Converts a modal formula to its propositional shadow."
  (if (is-modal? formula)  
      (intern (princ-to-string formula))
      formula))

(defun shadow-all (Formulae)
  (mapcar (lambda (formula) (shadow-formula formula)) 
          Formulae))

(defun filter (pred sequence &rest args)
  (apply #'remove-if (append (list (complement pred) sequence)
                             args)))

(defun common-knowledge? (formula) 
  (optima:match formula
    ((list 'common _ _) t) 
    (_ nil)))

(defun knowledge? (formula) 
  (optima:match formula
    ((list 'knows _ _ _) t) 
    (_ nil)))

(defun and? (formula) 
  (optima:match formula
    ((list 'and _ _) t) 
    (_ nil)))

(defun or? (formula) 
  (optima:match formula
    ((list 'or _ _) t) 
    (_ nil)))

(defun implies? (formula) 
  (optima:match formula
    ((list 'implies _ _) t) 
    (_ nil)))

(defun agents (formula)
  (optima:match formula
    ((or (list 'knows a _ F)
         (list 'believes a _ F)) 
     (cons a (agents F)))
    (_ ())))


(defun times (formula)
  (optima:match formula
    ((or (list 'common time F) 
         (list 'knows time _ F)
         (list 'believes time _ F)) 
     (cons time (times F)))
    (_ ())))

(defun multiply (x lists)
  (mapcar (lambda (list) (cons x list)) lists))

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


(defun apply-rule (rule &rest args)
  (case rule
    (:and-elim (optima:match (first args) ((list 'and A B) (list A B))))
    (:implies-elim (optima:match (first args) 
                     ((list (list 'implies _ Cons) _) Cons)))
    (:R1 (optima:match args ((list (list 'C _ F)
                                   a1 a2 a3 t1 t2 t3)
                             `(knows ,a1 ,t1 (knows ,a2 ,t2 (knows ,a3 ,t3 ,F))))))
    (:R4 (optima:match (first args) ((list 'knows _ _ F) F)))
    (otherwise (error "~ unimplemented rule." rule))))



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
                             (list (princ-to-string (first unused-and-elim-args)))  
               )))))

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


(defun forward (Premises Formula &optional (proof-stack nil))
    (or (handle-R4 Premises Formula proof-stack)
        (handle-and-elim Premises Formula proof-stack)
        (handle-implies-elim Premises Formula proof-stack)
        (handle-or-elim Premises Formula proof-stack)))

(defun prove (Premises Formula &optional (proof-stack nil) )
  (if (prove-from-axioms (shadow-all Premises) formula :time-limit 10 :verbose nil) 
       (add-to-proof-stack proof-stack :FOL Formula) 
      (forward Premises Formula proof-stack)))



;;; show code

(defun draw-node (string) 
  (gtfl:who 
   (:div :style "padding:4px;border:1px solid #888;margin-top:4px;margin-bottom:4px;background-color:#eee;"
         (princ string))))

(defun draw-tree* (tree) 
  (gtfl:draw-node-with-children  
   (gtfl:who-lambda (draw-node (car tree)))
   (mapcar #'(lambda (x) (gtfl:who-lambda (draw-tree* x))) (cdr tree))))

(defun show (proof)
  (labels ((convert-to-string-tree (x)
             (cond ((null x) ())
                   ((atom x) (string-downcase (princ-to-string x)))
                   (t  (mapcar #'convert-to-string-tree (remove nil x) )))))
    (gtfl:gtfl-out (draw-tree* (convert-to-string-tree proof)))))
