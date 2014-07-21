(in-package :shadowprover)

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


(defun modal-F (formula)
  (optima:match formula
    ((or (list 'common _ F) 
         (list 'knows _ _ F)
         (list 'believes _ _ F)) 
     F)
    (_ nil)))


(defun consequent (F) (second (args F)))
