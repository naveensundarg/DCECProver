
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

(defun multiply (x lists)
  (mapcar (lambda (list) (cons x list)) lists))
