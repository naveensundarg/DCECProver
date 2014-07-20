

(in-package  :snark-interface)
 
;;(cl::make-snark-system)
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
  (if (not verbose) (snark-deverbose))
  (snark:run-time-limit time-limit)
  (snark:assert-supported t)
  (snark:assume-supported t)
  (snark:prove-supported t)
  (snark:use-resolution t)
  (snark:use-paramodulation t)
  (snark:allow-skolem-symbols-in-answers nil))

(defun prove-from-axioms (axioms f &key (time-limit 5) (verbose nil))
  (setup-snark :time-limit time-limit :verbose verbose)
  (mapcar #'snark::assert (mapcar #'!@ axioms))
  (if (equalp :PROOF-FOUND (snark:prove (!@ f)))
      t nil))


(defun consistent? (statements time)
  (not (prove-from-axioms statements '(and P (not P)) :time-limit time)))

(defun !@ (x) 
  "reading logic forms with the symbols in the correct package"
  (let ((*package* (find-package :snark)))
    (read-from-string (princ-to-string x))))
 

