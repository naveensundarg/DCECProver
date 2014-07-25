;;;; ShadowProver.lisp

(in-package #:shadowprover)


(defun declare-default-sorts ()
  (snark:declare-sort 'Object)
  (snark:declare-sort 'snark::Action)
  (snark:declare-sort 'snark::ActionType)
  (snark:declare-sort 'snark::Fluent)
  (snark:declare-sort 'snark::Agent)
  (snark:declare-subsort 'snark::Moment 'snark::Number))


(defun declare-default-functors ())


(defun true? (x) (prove-from-axioms nil x))
(defun false? (x) (prove-from-axioms nil `(not ,x)))

(defun declarer-sorts-and-functors (sorts subsorts functions relations )
  (lambda () (declare-default-sorts)
          (declare-default-functors)
          (mapcar #'snark:declare-sort sorts)
          (mapcar (lambda (s) (apply #'snark:declare-subsort s)) subsorts)
          (mapcar (lambda (s) (apply #'snark:declare-function s)) functions)
          (mapcar (lambda (s) (apply #'snark:declare-relation s)) relations)))

(defun apply-rule (rule &rest args)
  (case rule
    (:and-elim (optima:match (first args) ((list 'and A B) (list A B))))
    (:implies-elim (optima:match (first args) 
                     ((list (list 'implies _ Cons) _) Cons)))
    (:R1 (optima:match args ((list (list 'C _ F)
                                   a1 a2 a3 t1 t2 t3)
                             `(knows ,a1 ,t1 (knows ,a2 ,t2 (knows ,a3 ,t3 ,F))))))
    (:DR6 (optima:match (first args) 
            ((list (list 'knows a time F) _)
             `(knows ,a ,time ,(consequent F)))))
    (otherwise (error "~ unimplemented rule." rule))))

(defun forward (Premises Formula sortal-fn &optional (proof-stack nil))
    (try 
     (lambda (fn) (funcall fn Premises Formula sortal-fn proof-stack))
     (mapcar #'symbol-function 
             '(handle-DR2 
               handle-DR3
               handle-DR4
               handle-DR5
               handle-DR6
               handle-DR9
               handle-R4
               handle-and-elim
               handle-implies-elim
               introduce-theorems
               handle-or-elim
               handle-reductio))))

 

(defparameter *debug* nil)
(defun debug-prove (Premises Formula caller)
  (format t "Total Premises: ~a | Formula: ~a | Caller: ~a ~%" (length
                                                                Premises)
          Formula caller))
(defparameter *sorts* nil)

(defun make-shadow-declarations (shadows)
  (mapcar (lambda (s) `(,s 0)) shadows))

(defun concatfn (f g) (lambda () (if f (funcall f)) (if g (funcall g))))


(defun prove (Premises Formula &key 
                                 (sorts nil) 
                                 (subsorts nil)
                                 (functions nil)
                                 (relations nil)
                                 (proof-stack nil) (caller nil))
  (let ((sortal-fn (declarer-sorts-and-functors sorts
                                                subsorts
                                                functions
                                                relations)))
    (prove! Premises Formula :sortal-fn sortal-fn)))


(defun prove! (Premises Formula &key 
                                 sortal-fn
                                  
                                 (proof-stack nil) (caller nil))
  (if *debug* (debug-prove Premises Formula caller))
  (if  (multiple-value-bind (shadowed shadows) 
           (shadow-all (cons Formula Premises))
         (let ((sortal-setup   
                (concatfn sortal-fn 
                          (lambda () 
                            (mapcar (lambda (s) (apply #'snark:declare-relation
                                 s))
                                    (make-shadow-declarations shadows))))))
           (prove-from-axioms (rest shadowed) (first shadowed) 
                              :time-limit 2 
                              :verbose nil :sortal-setup-fn sortal-setup))) 
       (add-to-proof-stack proof-stack :FOL Formula) 
       (forward Premises Formula sortal-fn proof-stack )))



(defun time-fn ())

;;; show code

;((defun )

