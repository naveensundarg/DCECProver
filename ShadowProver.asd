;;;; ShadowProver.asd

(asdf:defsystem #:shadowprover
  :serial t
  :description "Describe ShadowProver here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:optima #:fiveam #:gtfl)
  :components ((:file "package")
               (:file "snark-interface")
               (:file "utils")
               (:file "folrules")
               (:file "primitiverules")
               (:file "derivedrules")
               (:file "shadowprover")
               (:file "show")
               (:file "./tests/dev-tests")
               (:file "./projects/akrasia")))

