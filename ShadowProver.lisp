;;;; ShadowProver.lisp

(in-package #:shadowprover)


(defparameter *tackled-backwards* nil)
(defparameter *tackled-implies* nil)
(defparameter *expanded* nil)
(defparameter *premises* nil)
(defparameter *fol-counts* 0)
(defparameter *modal-counts* 0)
(defparameter *line-number* 1)
(defparameter *debug* nil)
(defclass proof ()
  ((proof :accessor proof
          :initform 'proof
          :initarg :proof)))

(defmethod print-object ((object proof) stream)
  (format stream "~a" (proof object)))

(defun declare-default-sorts ()
  (snark:declare-sort 'snark::Obj)
  (snark:declare-sort 'snark::Action)
  (snark:declare-sort 'snark::ActionType)
  (snark:declare-sort 'snark::Object)
  (snark:declare-sort 'snark::Fluent)
  (snark:declare-sort 'snark::Agent)
  (snark:declare-sort 'snark::Moment))


(defun declare-default-functors ())


(defun true? (x) (first (prove-from-axioms nil x)))
(defun false? (x) (first (prove-from-axioms nil `(not ,x))))

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


(defun backward (Premises Formula sortal-fn &optional (proof-stack nil))
 (try 
      (lambda (fn) 
        (if *debug* (print fn))
          (funcall fn Premises Formula sortal-fn proof-stack))
      (mapcar #'symbol-function 
              '(backward-DR1
                backward-DR2))))

(defun forward (Premises Formula sortal-fn &optional (proof-stack nil))
  (setf *modal-counts* (+ *modal-counts* 1)) (try 
      (lambda (fn) 
         (if *debug* (print fn))
          (funcall fn Premises Formula sortal-fn proof-stack))
      (mapcar #'symbol-function 
              *forward-proof-calculus*)))

 

(defparameter *debug* nil)
(defun debug-prove (Premises Formula caller)
  (format t "Total Premises: ~a | Formula: ~a | Caller: ~a ~%" (length
                                                                Premises)
          Formula caller))
(defparameter *sorts* nil)

(defun make-shadow-declarations (shadows)
  (mapcar (lambda (s) `(,s 0)) shadows))

(defun concatfn (f g) (lambda () (if f (funcall f)) (if g (funcall g))))



(defun prove (Premises Formula  &key 
                                  (signature *signature*)
                                  (verbose nil)
                                  (sorts nil) 
                                  (subsorts nil)
                                  (functions nil)
                                  (relations nil)
                                  (proof-stack nil) (caller nil))
;  (sb-ext:gc :full t)
  (setf *snark-verbose* verbose)
  (let* ((*signature* signature)
         (*line-number* 0)
         (*tackled-backwards* nil)
         (*tackled-implies* nil)
         (sortal-fn (declarer-sorts-and-functors sorts
                                                subsorts
                                                functions
                                                relations))
        (found  (prove! Premises Formula :sortal-fn sortal-fn)))
    (if found (dseq found))))


(defparameter *prover-done* nil)
(defparameter *prover-result* nil)
(defparameter *prover-lock* (bordeaux-threads:make-recursive-lock "prover-lock"))

(defun threaded-prover (axioms f &key (time-limit 5) (verbose nil)
                                     sortal-setup-fn)
  (setf *prover-result* nil)
  (setf *prover-done* nil)
  (bordeaux-threads:make-thread 
   (lambda ()
     (if (first (prove-from-axioms axioms f 
                                   :time-limit time-limit
                                   :verbose verbose :sortal-setup-fn sortal-setup-fn))
         (setf *prover-result* t))
     (setf *prover-done* t)))
  (dotimes (x (floor (/ time-limit .01)) *prover-result*)
    (sleep 0.02)
    (if *prover-done*
         (return *prover-result*))))

(defun shadow-prover (Premises Formula &key 
                                 sortal-fn
                                 (proof-stack nil) (caller nil))
  (setf *fol-counts*  (+ 1 *fol-counts*))
    (multiple-value-bind (shadowed shadow-table) 
        (shadow-all (cons Formula Premises))
      (let ((sortal-setup   
             (concatfn sortal-fn 
                       (lambda () 
                         (mapcar (lambda (s) (apply #'snark:declare-relation
                                                    s))
                                 (make-shadow-declarations (maphash (lambda
                                                                        (key
                                 value)
                                                                      (declare
                                 (ignore key)) value) shadow-table)))))))
        (let ((ans (prove-from-axioms (rest shadowed) (first shadowed) 
                                      :time-limit 0.1
                                      :verbose nil :sortal-setup-fn
                                      sortal-setup)))
          (if (proved? ans)
              (list t 
                    (let ((prems (used-premises ans)))
                      (maphash
                       (lambda (key value)
                         (setf prems (subst key value prems :test
                                            #'equalp)))
                       shadow-table) prems)))))))

(defun str* (base n) 
  (let ((str "")) 
    (loop for i from 1 to n do 
	 (setf str (concatenate 'string str base)))
    str))


(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun interactive-interface (info)
  (incf *line-number*)
  (let ((command (prompt-read 
		  (concatenate 'string 
			       (princ-to-string *line-number*) 
			       ":"
                               (princ-to-string info)))))))



(defun prove! (Premises Formula &key 
                                 sortal-fn
                                  
                                 (proof-stack nil) (caller nil))
  (if (or *debug* *interactive*) 
      (interactive-interface caller))
  (if *debug*  (progn (print Premises) (print Formula)))
  (let ((shadow-ans (shadow-prover Premises Formula
                        :sortal-fn sortal-fn :proof-stack
                        proof-stack :caller caller)))
      (if  shadow-ans
           (add-to-proof-stack proof-stack :FOL Formula (used-premises shadow-ans)) 
       (or
        (if (not (elem Formula *tackled-backwards*)) 
            (let ()
              (setf *tackled-backwards* (cons Formula *tackled-backwards*))
              (backward Premises Formula sortal-fn proof-stack)))
        (let () 
          (setf *tackled-backwards* (cons Formula *tackled-backwards*))
          (forward  Premises Formula sortal-fn proof-stack))))))



(defun run-all-tests ()
(5am:explain! (5am:run 'shadowprover::shadowprover-dev-tests))
)

