;;;; ShadowProver.asd

(asdf:defsystem #:shadowprover
  :serial t
  :description "Describe ShadowProver here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:optima #:fiveam)
  :components ((:file "package")
               (:file "snark-interface")
               (:file "shadowprover")
               (:file "./tests/dev-tests")))

