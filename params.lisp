(in-package :shadowprover)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Parameters ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *signature* nil)

(defparameter *interactive* nil)
(defparameter *snark-verbose* t)
(defparameter *optimize* nil)
(defparameter *tackled-backwards* nil)
(defparameter *tackled-implies* nil)
(defparameter *tackled-disjuncts* nil)

(defparameter *expanded* nil)
(defparameter *premises* nil)
(defparameter *fol-counts* 0)
(defparameter *modal-counts* 0)
(defparameter *line-number* 1)
(defparameter *debug* nil)
