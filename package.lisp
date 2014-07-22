;;;; package.lisp


(defpackage #:snark-interface
  (:documentation "Abstracting over SNARK's wonky interface.")
  (:use #:cl)
  (:export  #:!@ :consistent? :prove-from-axioms))

(defpackage #:enumerations 
  (:documentation "Enumerating all terms of a certain depth given signature.")
  (:nicknames :enums)
  (:use #:cl)
  (:export :declare-signature :generate))

(defpackage #:shadowprover
  (:use #:cl #:optima #:enumerations #:snark-interface))

