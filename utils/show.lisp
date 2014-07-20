(in-package :shadowprover)

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
