(in-package :shadowprover)

(defun rule-node? (string)
  (equalp ">" (subseq string 0 1)))
(defun premise-node? (string)
  (equalp "PREMISE" string))
(defun draw-node (string) 
  (cond 
    ((rule-node? string)
     (gtfl:who 
      (:div
       :style 
       "padding:4px;border:2px solid #888;margin-top:4px;margin-bottom:4px;background-color:#4169E1;color:fff;"
       (princ (subseq string 1)))))
    ((premise-node? string)
     (gtfl:who 
      (:div
       :style 
       "padding:4px;border:2px solid #888;margin-top:4px;margin-bottom:4px;background-color:#32CD32;"
       (princ string))))
    (t
     (gtfl:who 
        (:div
         :style "padding:4px;border:2px solid #7B68EE;margin-top:4px;margin-bottom:4px;background-color:#ff0;color:222;"
         (princ string))))))

(defun draw-tree* (tree) 
  (gtfl:draw-node-with-children 
   (gtfl:who-lambda (draw-node (car tree)))
   (mapcar #'(lambda (x) (gtfl:who-lambda (draw-tree* x))) (cdr tree))
   :right-to-left t))

(defun show (proof)
  (labels ((convert-to-string-tree (x)
             (cond ((null x) ())
                   ((atom x) (string-downcase (princ-to-string x)))
                   (t  (mapcar #'convert-to-string-tree (remove nil x) )))))
    (gtfl:gtfl-out (draw-tree* (convert-to-string-tree proof)))))
