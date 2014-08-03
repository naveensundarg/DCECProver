;;;; ShadowProver.asd

(asdf:defsystem #:shadowprover
  :serial t
  :description "Describe ShadowProver here"
  :author "Naveen Sundar G."
  :license "Specify license here"
  :depends-on (#:optima #:fiveam #:gtfl)
  :components ((:file "package")
               (:file "./snark-20120808r022/snark-interface")
               (:file "./utils/misc")
               (:file "./utils/syntax")
               (:file "./utils/sorts")
               
               (:file "./inference-rules/expanders")
               (:file "./inference-rules/backward-rules")
               (:file "./inference-rules/folrules")
               (:file "./inference-rules/primitiverules")
               (:file "./inference-rules/derivedrules")

               (:file "./utils/show")
               
               (:file "configs")
               (:file "shadowprover")
               
               (:file "./tests/dev-tests")
               (:file "./projects/akrasia/akrasia")
               (:file "./projects/false-belief-task/false-belief-task")))

