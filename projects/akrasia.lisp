(in-package :shadowprover)
(5am:def-suite akrasia-tests)
(5am:in-suite akrasia-tests)

(defparameter *KB-selfd*
  (list
   '(forall 
         (a t1 t2)
     (implies (and (< t1 now)
               (< now t2))
          (iff 
           (believes I now (holds (harmed a I*) t1))
           (desires I now (holds (disabled I* a) t2)))))))


(defparameter *KB-deta*
  (list 
   '(believes I now 
         (ought I* time (holds (custody a I*) t)
          (happens (action I* (refrain (harm a))) time)))
   '(K I now (holds (detainee s) tf))
   '(forall (time) 
     (knows I now (implies 
               (holds (detainee s) time)
               (holds (custody s I*) time))))))

(defparameter *KB-rs*
  (list
   '(knows I now (holds (harmed s I*) tp))
   '(forall (a time)
         (implies 
          (desires I now (holds (disabled I* a) time))
          (intends I now (happens (action I* (harm a)) time))))
   '(knows I t1 (forall ((act ActionType) (t1 Moment) (t2 Moment))
                 (iff 
                  (happens (action I* (refrain act) t2))
                  (not (happens (action I*  act t2))))))
   '(< tp now)
   '(< now tf)))


(defparameter *KB-es*
  (list
   '(forall (a time)
         (implies 
          (holds (custody a I) time)
          (not (happens (action I* (harm a)) time))))))



(defparameter *DCEC*
    (list '(= I I*)))


(defparameter *akrasia-simulation*
  (append 
   *KB-selfd* 
   *KB-deta*
   *KB-rs* 
   *DCEC*))

(sorts:declare-signature 
 *signature*
 (:name S :output agent :inputs nil)
 (:name I :output agent :inputs nil)
 (:name damaged :output fluent :inputs (agent))
 (:name kick :output actiontype :inputs (agent))
 (:name harm :output actiontype :inputs (agent))
 (:name initiates :output boolean :inputs (action fluent moment))
 (:name happens :output boolean :inputs (action moment))
 (:name holds :output boolean :inputs (fluent moment))
 (:name tp :output moment :inputs nil)
 (:name tf :output moment :inputs nil)
 (:name now :output moment :inputs nil))

(5am:test D2
  (5am:is-true
   (prove *akrasia-simulation*
		     '(believes I now (holds (harmed s I*) tp)))))

(5am:test D3
  (5am:is-true
   (prove *akrasia-simulation*
		     '(iff 
		       (happens (action I* (refrain (harm S)) tf))
		       (not (happens (action I* (harm S) tf)))))))

