
(in-package #:shadowprover)

(defun try (f cases)
  (if (null cases)
      nil
      (let ((first-try (funcall f (first cases))))
	(if first-try (values first-try (first cases)) (try f (rest cases))))))


;; (defun elem (e set) 
;;   (declare (ignore set))
;;   (let ((present (gethash e *expanded*)))
;;     (if present present (progn  (setf (gethash e *expanded*) t) nil))))
(defun elem (e set) (member e set :test #'equalp))
(defun args (F) (rest F))
(defun cartesian-product (sets)
  (if ( < (length sets) 2) 
      (mapcar (lambda (x) (list x)) (first sets))
      (let ((rst (cartesian-product (rest sets))))
        (apply #'append 
               (mapcar (lambda (x) (mapcar (lambda (y) (cons x y)) rst)) 
                       (first sets))))))

(defun cartesian-power (set n)
  (labels ((n-tuple (set n)
             (if (zerop n) 
                 ()
                 (cons set (n-tuple set (1- n))))))
    (cartesian-product (n-tuple set n))))
(defun permute (seq)
  (cond ((null seq) '())
        ((= 1 (length seq)) (list seq))
        (t (reduce #'append (mapcar (lambda (element) 
		     (mapcar (lambda (l) (cons element l))
                             (permute (remove element seq))))
		   seq)))))

(defun multiply (x lists)
  (mapcar (lambda (list) (cons x list)) lists))

(defun k-to-b (f)
  (and (knowledge? f) (list 'believes 
                            (modal-agent f)
                            (modal-time f)
                            (modal-F f))))

 
(defun subs (f)
  (cond ((atom f) (list f))
        ((knowledge? f) (cons (k-to-b f) (list (subs (modal-F f)))))
        ))
