
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
