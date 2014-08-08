(in-package :shadowprover)

(defun justification (step) (first step))
(defun out (step) (third step))
(defmethod outp (proof) (out (first (last proof))))

(defun find-step (proof formula) 
  (first (remove-if-not (lambda (step) (equalp formula
                                               (out
                                                step)))
                        proof)))
(defun premises (step) (second step))

(defun premises* (steps)
  (remove-duplicates
   (reduce #'append (mapcar (lambda (step) 
                              (if (atom (justification step)) 
                                  (premises step) 
                                  (premises* (premises step)))) steps))
   :test #'equalp))

(defun justify (formula proof-stack)

  (let ((steps (remove-if-not (lambda (step) (equalp formula (out step))) proof-stack)))
    (if steps
        (cons  
         (first steps) 
         (reduce #'append 
                 (mapcar (lambda (premise) (justify premise (remove (first steps) proof-stack)))
                               (premises (first steps))))))))

(defun proof-to-tree (proof formula)
  (if (not (null proof))
      (let* ((step (find-step proof formula))
            (justification (justification step))
            (premises (premises step)))
        (list (princ-to-string formula)
              (if step 
                  (cons (concatenate 'string ">" (princ-to-string justification))
                        (mapcar (lambda (premise) 
                                  (proof-to-tree proof premise)) premises))
                  (list "PREMISE"))))))

(defun remove-duplicated-sub-trees (tree &optional 
                                           (trees (make-hash-table
  :test #'equalp)))
  (if (zerop (hash-table-size trees)) (reset-counter "proof-"))
   (labels ((name (x) (first x))
           (children (x) (rest x)))
     (if (and (gethash tree trees) (not (premise-node? (name tree))))
         (list (concatenate 'string "Node [" (princ-to-string (gethash tree
                                           trees)) "]"))
         (let ((curr-id (next-counter "proof-")))
           (setf (gethash tree trees) 
                 curr-id)
              (cons (concatenate 'string (name tree)
                                "[" (princ-to-string curr-id) "]") 
                    (mapcar (lambda (child) (remove-duplicated-sub-trees child
                                                                         trees))
                            (children tree)))))))

(defun trim (proof)
  (justify (out (first proof)) proof))

(defun dseq (proof-stack)  (reverse (trim proof-stack)))
(defun add-to-proof-stack (proof-stack rule out args)
  (if *optimize*
      t
      (cons  (list rule args out) proof-stack)))
(defun is-modal? (F)
  (optima:match F 
    ((or
      (list 'common _ _)
      (list 'knows _ _ _) 
      (list 'desires _ _ _)             
      (list 'sees _ _ _)
      (list 'believes _ _ _)) t)
    (_ nil)))

(defun connective (F) (first F))
(defparameter *shadows* (make-hash-table))

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
        (setf (gethash formula *shadows*) shadow)
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
  (let ((*shadows* (make-hash-table))) 
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

(defun belief? (formula) 
  (optima:match formula
    ((list 'believes _ _ _) t) 
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
             ((list (or 'believes 'knows 'desires) agent time F) 
              (append (list agent) (list time)
                      (terms agent) (terms time)
                      (handle-complex-vs-atom F)))
             ((list (or 'and 'or 'implies 'iff) P1 P2) 
              (append (terms P1)
                      (terms P2)))
             ((list 'not P) 
              (handle-complex-vs-atom P))
             ((list (or 'forall 'exists) vars P) 
              (set-difference (handle-complex-vs-atom P) (strip-away-sorts vars) :test #'equalp))
             ((guard x (compound? x)) 
              (reduce #'append (mapcar #'subs (rest x))))
             ((guard x (atom x)) (handle-complex-vs-atom x)))))
      (remove-duplicates terms-with-dupes :test #'equalp))))

(defun terms* (formulae)
  (remove-duplicates (reduce #'append (mapcar #'terms formulae))))


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


(defun k-to-b (f)
  (and (knowledge? f) (list 'believes 
                            (modal-agent f)
                            (modal-time f)
                            (modal-F f))))

 
(defun subs (f)
  (cond ((atom f) (list f))
        ((knowledge? f) (cons (k-to-b f) (list (subs (modal-F f)))))))
