(in-package :shadowprover)

(defun add-to-proof-stack (proof-stack rule out &rest args)
  (list (princ-to-string out) (append (list rule proof-stack) args)))
(defun is-modal? (F)
  (optima:match F 
    ((or
      (list 'common _ _)
      (list 'knows _ _ _) 
      (list 'sees _ _ _)
      (list 'believes _ _ _)) t)
    (_ nil)))

(defun connective (F) (first F))
(defparameter *shadows* nil)

(defun make-name (formula)
  (cl-ppcre:regex-replace-all "[\\\s]"
   (cl-ppcre:regex-replace-all  
    "[)]" 
    (cl-ppcre:regex-replace-all  "[(]" (princ-to-string formula) "")
    "") ""))
(defun shadow-formula (formula)
  "Converts a modal formula to its propositional shadow."
  (if (is-modal? formula)  
      (let ((shadow (make-symbol (make-name formula))))
        (setf *shadows* (cons shadow *shadows*))
        shadow)
      (optima:match formula 
        ((cons (or 'and 'or 'implies 'iff) args)
         (cons (connective formula)
               (mapcar #'shadow-formula args)))
        ((list 'not P)
         (list 'not
               (shadow-formula P)))
        ((list (or 'forall 'exists) vars P)
         (list (quantifier formula) vars
               (shadow-formula P)))
        (_ formula))))

(defun shadow-all (Formulae)
  (let ((*shadows* nil)) 
    (let ((shadowed (mapcar (lambda (formula) (shadow-formula formula)) 
                            Formulae)))
      (values shadowed *shadows*))))

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

(defun universal? (formula) 
  (optima:match formula
    ((list 'forall _ _) t) 
    (_ nil)))
(defun agents (formula)
  (optima:match formula
    ((or (list 'knows a _ F)
         (list 'believes a _ F)) 
     (remove-duplicates (cons a (agents F))))
    ((list 'not F) (agents F))
    ((list _ A B) (append (agents a) (agents b)))
    (_ nil)))

(defun agents* (formulae)
  (remove-duplicates 
   (reduce #'append (mapcar #'agents formulae))))

(defun agent-tuples (formulae n)
  (let ((agents (agents* formulae) ))
    (cartesian-power agents n)))

(defun times (formula)
  (optima:match formula
    ((or (list 'common time F) 
         (list 'knows time _ F)
         (list 'believes time _ F)) 
     (cons time (times F)))
    (_ ())))


(defun modal-agent (formula)
  (optima:match formula
    ((or (list 'knows a _ _)
         (list 'believes a _ _)) 
     a)
    (_ nil)))

(defun modal-time (formula)
  (optima:match formula
    ((or (list 'common time _) 
         (list 'knows _ time _)
         (list 'believes _ time _)) 
     time)
    (_ nil)))

(defun modal-F (formula)
  (optima:match formula
    ((or (list 'common _ F) 
         (list 'knows _ _ F)
         (list 'believes _ _ F)) 
     F)
    (_ nil)))

(defun antecedent (F) (first (args F)))
(defun consequent (F) (second (args F)))

(defun compound-F? (formula)
  (optima:match formula 
      ((or (list (or 'or 'and 'implies 'iff) _ _)
           (list (or 'knows 'believes) _ _ _)
           (list 'not _)
           (list (or 'forall 'exists) _ _)) t)))

(defun compound? (formula) (not (atom formula)))
 
(defun subs (f)
  (if (atom f)
      (list f)
      (cons f (reduce #'append (mapcar #'subs (rest f))))))
(defun sorted? (F) 
  (optima:match F
    ((list (or 'forall 'exists) vars _) 
     (some (lambda (s) (listp s)) vars))))
(defun var-sorts (vars) 
  (mapcar (lambda (s) (second s)) vars))


(defun strip-away-sorts (vars) 
  (mapcar (lambda (s) (if (atom s) s (first s))) vars))
(defun terms (formula)
  (labels ((handle-complex-vs-atom (P)
             (if (compound-F? P) 
                 (terms P) 
                 (if (not (atom P)) (reduce #'append (mapcar #'subs (rest P)))))))
    (let ((terms-with-dupes 
           (optima:match formula
             ((list 'common agent F) 
              (append (list agent)
                      (terms agent) 
                      (handle-complex-vs-atom F)))
             ((list (or 'believes 'knows) agent time F) 
              (append (list agent) (list time)
                      (terms agent) (terms time)
                      (handle-complex-vs-atom F)))
             ((list (or 'and 'or 'implies 'iff) P1 P2) 
              (append (handle-complex-vs-atom P1)
                      (handle-complex-vs-atom P2)))
             ((list 'not P) 
              (handle-complex-vs-atom P))
             ((list (or 'forall 'exists) vars P) 
              (set-difference (handle-complex-vs-atom P) (strip-away-sorts vars) :test #'equalp))
             ((guard x (compound? x)) 
              (reduce #'append (mapcar #'subs (rest x))))
             ((guard x (atom x)) (handle-complex-vs-atom x)))))
      (remove-duplicates terms-with-dupes :test #'equalp))))

(defun terms* (formulae)
  (reduce #'append (mapcar #'terms formulae)))


(defun quantifier (quantifiedF)
  (optima:match quantifiedF 
    ((list 'forall _ _) 'forall)
    ((list 'exists _ _) 'exists)))

(defun conn (F)
  (optima:match F 
    ((list (or 'and 'or 'iff 'implies) _ _) (first F))
    ((list 'not _) 'not)))

(defun kernel (quantifiedF)
  (optima:match quantifiedF 
    ((list (or 'forall 'exists) _ K) K)))

(defun top-var (quantifiedF)
  (optima:match quantifiedF 
    ((list (or 'forall 'exists) vars _) 
     (if (atom (first vars))
         (first vars)
         (first (first vars))))))

(defun rest-vars (quantifiedF)
  (optima:match quantifiedF 
    ((list 'forall vars _) (rest vars))
    ((list 'exists vars _) (rest vars))))

(defun vars (quantifiedF)
  (optima:match quantifiedF 
    ((list (or 'exists'forall) vars _) vars)))
(defun specialize (Univ term)
  (if (rest-vars Univ)
      (list (quantifier Univ ) 
            (rest-vars Univ) 
            (subst term  (top-var Univ) (kernel Univ)))
      (subst term (top-var Univ) (kernel Univ))))
