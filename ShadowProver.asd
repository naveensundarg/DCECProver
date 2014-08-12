;;;; ShadowProver.asd

(asdf:defsystem #:shadowprover
  :serial t
  :description "A prover for DCEC, a first-order modal logic."
  :author "Naveen Sundar G."
  :license "MIT BSD"
  :depends-on (#:optima #:fiveam #:gtfl)
  :components ((:file "package")
               (:file "configs")

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
               
               (:file "shadowprover")
               
               (:file "./tests/dev-tests")
               (:file "./projects/akrasia/akrasia")
               (:file "./projects/false-belief-task/false-belief-task")
               (:file "./projects/wise-men-puzzle/wise-men-puzzle")
               (:file "./tests/all-tests")))

