(in-package :shadowprover)

(declare-signature 
 *kk-signature*

 (:name f1 :output fluent :inputs nil)
  (:name f2 :output fluent :inputs nil)
 (:name a1 :output agent :inputs nil)
 (:name a2 :output agent :inputs nil)
 (:name alpha :output actiontype :inputs nil)
 (:name t :output moment :inputs nil)
 (:name t1 :output moment :inputs nil)
 (:name t2 :output moment :inputs nil))

;; Two individuals. a1 and a2 exactly one is a knight and the other knave
;; a1 says "we are both knaves."

(5am:def-suite knights-and-knaves)
(5am:in-suite  knights-and-knaves)

(5am:test kk-prob-1
  (5am:is-true
   (prove (list '(forall ((?a Agent) (?f Fluent) (?t Moment))
                  (iff (and (holds (knight ?a) t) (Says ?a now (holds ?f ?t)))
                   (holds ?f ?t)))
                '(forall ((?a Agent) (?f Fluent) (?t Moment))
                  (iff (and (holds (knave ?a) t) (Says ?a now (holds ?f ?t)))
                   (not (holds ?f ?t))))
                '(iff (holds (knight a1) t) (holds (knave a2) t))
                '(iff (holds (knight a2) t) (holds (knave a1) t))
                '(Says a1 now (holds (knave a1) t))
                '(Says a1 now (holds (knave a2) t))
                '(forall ((?a Agent)) (implies (holds (knight ?a) t)
                                       (not (holds (knave ?a) t))))
                '(forall ((?a Agent)) (implies (holds (knave ?a) t)
                                       (not (holds (knight ?a) t))))
                '(forall ((?a Agent)) (or (holds (knight ?a) t)
                                       (holds (knave ?a) t))))
          '(and (holds (knave a1) t) (holds (knight a2) t))
          :signature *kk-signature*)))
