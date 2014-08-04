

(in-package  :snark-interface)
 
;;(cl::make-snark-system)

(defun snark-verbose ()
  (snark:print-options-when-starting  nil)
  (snark:print-agenda-when-finished nil)
  (snark:print-clocks-when-finished t)
  (snark:print-final-rows nil)
  (snark:print-symbol-table-warnings nil)
  (snark:print-summary-when-finished t)
  (snark:print-row-answers nil)
  (snark:print-row-goals nil) 
  (snark:print-rows-when-derived nil)
  (snark:print-row-reasons nil)
  (snark:print-row-partitions nil)
  (snark:print-rows-prettily nil)
  (snark:print-rows :min 0 :max 0))

(defun snark-deverbose ()
  (snark:print-options-when-starting  nil)
  (snark:print-agenda-when-finished nil)
  (snark:print-clocks-when-finished nil)
  (snark:print-final-rows nil)
  (snark:print-symbol-table-warnings nil)
  (snark:print-summary-when-finished nil)
  (snark:print-row-answers nil)
  (snark:print-row-goals nil) 
  (snark:print-rows-when-derived nil)
  (snark:print-row-reasons nil)
  (snark:print-row-partitions nil)
  (snark:print-rows-prettily nil)
  (snark:print-rows :min 0 :max 0))

(defun setup-snark (&key (time-limit 5) (verbose nil))
  (snark:initialize :verbose  verbose)
  (if (not verbose) (snark-deverbose) )
  (snark:run-time-limit time-limit)
  (snark:assert-supported t)
  (snark:assume-supported t)
  (snark:prove-supported t)
  (snark:use-resolution t)
  (snark:use-paramodulation t)
  (snark:allow-skolem-symbols-in-answers nil))

(defun row-formula (name))
(defun prove-from-axioms (all-axioms f
                          &key 
                            (time-limit 5) 
                            (verbose nil)
                            sortal-setup-fn)
  (let ((axioms (remove-duplicates all-axioms :test #'equalp)))
    (setup-snark :time-limit time-limit :verbose verbose)
    (if sortal-setup-fn (funcall sortal-setup-fn))
    (let* ((n-a (make-hash-table :test #'equalp))
           (a-n (make-hash-table :test #'equalp)))
      (mapcar (lambda (axiom)
                (let ((name (gensym)))
                  (setf (gethash (princ-to-string axiom) a-n) name)
                  (setf (gethash (princ-to-string name) n-a) axiom))) axioms)
      (mapcar (lambda (axiom)
                (snark::assert axiom ;:name  ;(gethash (princ-to-string axiom) a-n)
                               ))
              (mapcar #'!@ axioms))
      (if (equalp :PROOF-FOUND (snark:prove (!@ f)))
          (list t (remove nil 
                          (mapcar 
                           (lambda (row reason)
                             (if (equalp reason 'snark::ASSERTION)
                                 (gethash (princ-to-string (snark:row-name row)) n-a )))
                           (snark:row-ancestry (snark:proof))
                           (mapcar 'snark:row-reason (snark:row-ancestry (snark:proof)))))) 
          (list nil nil)))))



(defun consistent? (statements time)
  (not (prove-from-axioms statements '(and P (not P)) :time-limit time)))

(defun !@ (x) 
  "reading logic forms with the symbols in the correct package"
  (let ((*package* (find-package :snark)))
    (read-from-string (princ-to-string x))))
 

