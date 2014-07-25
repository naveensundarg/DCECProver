;;;; package.lisp


(defpackage #:snark-interface
  (:documentation "Abstracting over SNARK's wonky interface.")
  (:use #:cl)
  (:export  #:!@ :consistent? :prove-from-axioms))

(defpackage #:sorts-system 
  (:documentation "Enumerating all terms of a certain depth given signature.")
  (:nicknames :sorts)
  (:use #:cl)
  (:export :declare-signature :get-sort :generate))

(defpackage #:shadowprover
  (:use #:cl #:optima #:sorts #:snark-interface))

