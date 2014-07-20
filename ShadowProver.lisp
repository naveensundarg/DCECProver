;;;; ShadowProver.lisp

(in-package #:shadowprover)


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
