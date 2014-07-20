;;;; package.lisp


(defpackage #:snark-interface
  (:documentation "Abstracting over SNARK's wonky interface.")
  (:use #:cl)
  (:export  #:!@ :consistent? :prove-from-axioms))

(defpackage #:shadowprover
  (:use #:cl #:optima #:snark-interface))

