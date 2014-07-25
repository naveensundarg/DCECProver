(load "/Users/naveen/work/Nucleus/snark-20120808r022/snark-system.lisp")
(make-snark-system)
(push "/Users/naveen/work/ShadowProver/" asdf:*central-registry*)
(asdf:load-system :shadowprover)


