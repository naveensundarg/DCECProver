(in-package :shadowprover)

(declare-signature 
 *signature*
  (:name a :output agent :inputs nil)
 (:name b :output agent :inputs nil)

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

(defparameter *M1-p1*
  '(sees a1 now (happens (action a2 alpha) t)))


(defparameter *A4*
  '(Common now (forall ((?a Agent) (?d ActionType) (?t Moment))
                (implies (happens (action ?a ?d) ?t)
                 (knows ?a ?t (happens (action ?a ?d) ?t))))))


(defparameter *A6*
  '(forall ((?a Agent) (?b Agent) (?t1 Moment) (?t2 Moment) (?f Fluent))
    (implies 
     (and 
      (believes ?a now (believes ?b now (holds ?f ?t1)))
      (believes ?a now (not (exists ((?e Event) (?t Moment))
                                (and (believes ?b now (happens ?e ?t))
                                     (believes ?b now (and (< ?t1 ?t) (< ?t ?t2)))
                                     (believes ?b now (terminates ?e ?f ?t)))))))
     (believes ?a now (not (believes ?b now (clipped ?t1 ?f ?t2)))))))


(defparameter *M2-P2*
  '(knows a1 a2 (holds f t1)))

(defparameter *M2-P3*
  '(and 
   (believes a1 now (believes a2 now (holds ?f ?t1))) 
   (believes a1 now (not (exists ((?e event) (?t moment)) 
                                (and (believes a2 now  (happens ?e ?t)) 
                                     (believes a2 now (and (< ?t1 ?t) (< ?t ?t2)))
                                     (believes a2 now (terminates ?e ?f ?t))))))))
(5am:def-suite false-belief-tests)
(5am:in-suite false-belief-tests)


(5am:test M1-2
  (5am:is-true
   (prove (list *M1-p1*)
          '(knows a1 now (happens (action a2 alpha) t)))))

(5am:test M1-3
  (5am:is-true
   (prove (list *M1-p1* *A4* )
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
   (prove (list *M1-p1* *A4* )
          '(knows a1 now (knows a2 t (happens (action a2 alpha) t))))))

(5am:test M2-2
  (5am:is-true
   (prove (list *M2-P2*)
		     '(knows a1 a2 (holds f t1)))))

(5am:test M2-3
  (5am:is-true 
   (prove (list *A6*)
          '(implies 
            (and 
             (believes a1 now (believes a2 now (holds ?f ?t1))) 
             (believes a1 now (not (exists ((?e event) (?t moment)) 
                                          (and (believes a2 now  (happens ?e ?t)) 
                                               (believes a2 now (and (< ?t1 ?t) (< ?t ?t2)))
                                               (believes a2 now (terminates ?e ?f ?t))))))) 
            (believes a1 now (not (believes a2 now (clipped ?t1 ?f ?t2))))))))



(5am:test M3-4
  (5am:is-true
   (prove (list *A6* *M2-P2* *M2-P3*)
          '(believes a1 now (not (believes a2 now (clipped ?t1 ?f ?t2)))))))
