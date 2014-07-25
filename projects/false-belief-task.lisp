(in-package :shadowprover)

(declare-signature 
 *signature*
 (:name a1 :output agent :inputs nil)
 (:name a2 :output agent :inputs nil)
 (:name alpha :output actiontype :inputs nil)
 (:name t :output moment :inputs nil)
 (:name action :output action :inputs (agent actiontpe))
 (:name initiates :output boolean :inputs (action fluent moment))
 (:name happens :output boolean :inputs (action moment))
 (:name holds :output boolean :inputs (fluent moment))
 (:name tp :output moment :inputs nil)
 (:name tf :output moment :inputs nil)
 (:name now :output moment :inputs nil))

(defparameter *p1*
  '(sees a1 now (happens (action a2 alpha) t)))


(defparameter *A4*
  '(Common now (forall ((?a Agent) (?d ActionType) (?t Moment))
                (implies (happens (action ?a ?d) ?t)
                 (knows ?a ?t (happens (action ?a ?d) ?t))))))



(5am:def-suite false-belief-tests)
(5am:in-suite false-belief-tests)


(5am:test M1-2
  (5am:is-true
   (prove (list *p1*)
          '(knows a1 now (happens (action a2 alpha) t)))))

(5am:test M1-3
  (5am:is-true
   (prove (list *p1* *A4* )
          '(knows a1 now (forall ((?a Agent) (?d ActionType) (?t Moment))
                          (implies (happens (action ?a ?d) ?t)
                           (knows ?a ?t (happens (action ?a ?d) ?t))))))))

(5am:test M1-4
  (5am:is-true (prove (list *A4* )
                      '(knows a1 now 
                        (implies (happens (action a2 alpha) t)
                         (knows a2 t (happens (action a2 alpha) t)))))))

(5am:test M1-5
  (5am:is-true 
   (prove (list *p1* *A4* )
          '(knows a1 now (knows a2 t (happens (action a2 alpha) t))))))
