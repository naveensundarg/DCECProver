;;;; ShadowProver.asd

(asdf:defsystem #:shadowprover
  :serial t
  :description "Describe ShadowProver here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:optima #:fiveam #:gtfl)
  :components ((:file "package")
               (:file "./snark-20120808r022/snark-interface")
               (:file "./utils/misc")
               (:file "./utils/syntax")
               (:file "./inference-rules/folrules")
               (:file "./inference-rules/primitiverules")
               (:file "./inference-rules/derivedrules")
               (:file "./utils/show")
               (:file "shadowprover")

               (:file "./tests/dev-tests")
               (:file "./projects/akrasia")))

