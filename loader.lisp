(defparameter *ql-modules*
  (list "fiveam" "optima" "gtfl"))

(mapcar #'quicklisp:quickload *ql-modules*)


(load "/Users/naveen/work/ShadowProver/snark-20120808r022/snark-system.lisp")
(make-snark-system)
(push (make-pathname :directory (pathname-directory *load-truename*)) asdf:*central-registry*)
(asdf:load-system :shadowprover)


