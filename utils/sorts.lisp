
(in-package :sorts)

(defun create-signature () ())
(defun add-to-signature (signature &key name output (inputs nil))
  (cons  (list name output inputs) signature))

(defun name (s) (first s))
(defun outputs (s) (second s))
(defun inputs (s) (third s))

(defun get-leaves (signature) 
  (remove-if  (lambda (s) 
                (not  (= 0 (length (inputs s)))))
             signature))

(defun cartesian-product (sets)
  (if ( < (length sets) 2) (mapcar (lambda (x) (list x)) (first sets))
      (let ((rst (cartesian-product (rest sets))))
        (apply #'append 
               (mapcar (lambda (x) (mapcar (lambda (y) (cons x y)) rst)) 
                       (first sets))))))

(defun multiply (term-sig terms)
  (let ((inp-sorts (inputs term-sig)))
    (mapcar (lambda (inputs) 
              (list `(,(name term-sig) ,@(mapcar #'name inputs)) (outputs term-sig)))
     (cartesian-product
      (mapcar 
       (lambda (inp-sort) 
         (remove nil
                 (mapcar (lambda (term)
                           (if  (equalp inp-sort (outputs term)) term nil)) 
                         terms)))
       inp-sorts)))))

(defun multiply-all (signature terms)
  (apply #'append (mapcar (lambda (term-sig) (multiply term-sig terms))
                          signature))) 

(defun generate-at-depth (signature depth)
  "Generates terms of a certain depth (can be unbalanced)."
  (if (= depth 0)
      (mapcar #'butlast (get-leaves signature))
      (let ((prev (generate-at-depth signature (1- depth))))
        (remove-duplicates (append prev (multiply-all signature prev)) :test #'equalp))))

(defun generate-int (signature currdepth maxdepth)
  (if (< maxdepth currdepth)
      nil
      (let ((curr-level-terms (generate-at-depth signature currdepth)))
        (if curr-level-terms
            (remove-duplicates
             (append  curr-level-terms (generate-int signature (1+ currdepth)
  maxdepth))
             :test #'equalp)))))

(defun generate (signature depth &optional (sort nil))
  (let ((generated (generate-at-depth signature  depth)))
    (mapcar #'name 
            (if sort
                (remove-if (complement (lambda (term) (equalp sort (outputs term)))) generated)
                generated))))

(defmacro declare-signature (name &rest decls)
  "Example: (declare-signature *simple-signature* 
                 (:name jack :output person :inputs nil)
                 (:name apple :output object :inputs: nil)
                 (:name son :output person :inputs (person))"
  `(defparameter ,name (reduce (lambda (signature decl)
                           (apply #'add-to-signature (cons signature decl)))
                         ',decls :initial-value (create-signature))))

(defun form-signature (&rest decls)
  "Example: (form-signature 
                 (:name jack :output person :inputs nil)
                 (:name apple :output object :inputs nil)
                 (:name son :output person :inputs (person))"
  (reduce (lambda (signature decl)
            (apply #'add-to-signature (cons signature decl)))
          decls :initial-value (create-signature)))

(defun args (term) (if (listp term) (rest term) nil))
(defun functor (term) (if (atom term) term (first term)))

(defun type-check (term signature)
  (and (every (lambda (arg) (type-check arg signature)) (args term))
   (equalp (get-input-sort term signature) 
           (mapcar (lambda (a) (get-sort a signature)) (args term)))))

(defun get-sort (term signature)
  (outputs (find term signature :test 
                 (lambda (x y) (equalp 
                                (functor x) (name y))))))

(defun get-input-sort (term signature)
  (inputs (find term signature :test 
                 (lambda (x y) (equalp 
                                (functor x) (name y))))))
 
;;;; [=|(=[=]=)|=] example from our IACAP paper
(sorts:declare-signature 
 *prob-1-sig*	  
 (:name S :output agent :inputs nil)
 (:name I :output agent :inputs nil)
 (:name damaged :output fluent :inputs (agent))
 (:name kick :output action :inputs (agent))
 (:name harmed :output action :inputs (agent))
 (:name initiates :output boolean :inputs (action fluent moment))
 (:name happens :output boolean :inputs (action moment))
 (:name holds :output boolean :inputs (fluent moment))
 (:name tp :output moment :inputs nil)
 (:name now :output moment :inputs nil))

(get-sort '(kick I)  *prob-1-sig*)
