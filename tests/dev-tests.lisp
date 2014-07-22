(in-package :shadowprover)
(5am:def-suite shadowprover-dev-tests)
(5am:in-suite shadowprover-dev-tests)

(5am:test dt1
  (5am:is-true (prove nil '(and (implies p p) (or p (not p))))))

(5am:test dt2
  (5am:is-true 
   (prove (list 
           '(implies (exists (x) (implies (Bird x) (forall (y) (Bird y))))
             (knows jack now Bird-Theorem)))
          '(knows jack now Bird-Theorem))))

(5am:test dt3
  (5am:is-true 
   (prove (list 
           '(knows jack now Bird-Theorem))
          'Bird-Theorem)))

(5am:test dt4
  (5am:is-true 
   (prove (list 
           '(knows a1 t1 (implies H (and E D)))
           '(knows a2 t2 (implies (or E My) R))
           '(knows a3 t2 (implies Ma (not R))))
          '(implies H (not Ma)))))

(5am:test dt5
  (5am:is-true 
   (prove (list 
           '(knows a1 t1 (implies H (and E D)))
           '(knows a1 t1 (knows a2 t2 (implies (or E My) R)))
           '(knows a1 t1 (knows a2 t2 (knows a3 t2 (implies Ma (not R))))))
          '(implies H (not Ma)))))

(5am:test dt6
  (5am:is-true
   (prove (list 
           
           '(and P (knows a now Q)))
          'Q)))

(5am:test dt7
  (5am:is-true
   (prove (list 
           '(and P (knows a now (and Q (knows b now R1))))
           '(and P (knows a now (and Q (knows b now R2)))))
          '(and R1 R2))))


(5am:test dt8
  (5am:is-true
   (prove (list 'P
                '(implies P (knows a now Q)))
          'Q)))


(5am:test dt9
  (5am:is-true
   (prove (list '(holds raining now)
                '(forall (a t) (implies (holds (bored a) t)
                                       (holds (sleepy a) t)))
                '(implies (holds raining now) 
                  (and (holds (drenched jack) now) 
                   (knows jack now (holds (bored jack) now)))))
          '(and 
            (holds (sleepy jack) now) 
            (holds (bored jack) now)
            (holds (drenched jack) now)))))

(5am:test dt10
  (5am:is-true
   (prove '((or (Knows a now P) (Knows b now P))
            (implies (not (Knows a now P)) (Knows b now P))
            (implies (not (Knows b now P)) (Knows a now P)))
          'P)))


(5am:test dt11
  (5am:is-true
   (prove '((or (Knows a now P) (Knows b now P))
            (implies (not (Knows a now P)) (Knows b now P))
            (implies (not (Knows b now P)) (Knows a now P))
            (implies P (Knows b now (and  (knows c t1 Q1)
                                          (knows c t2 Q2)))))
          '(and Q1 Q2))))

(5am:test dt12 
  "Lemma 6 from http://kryten.mm.rpi.edu/arkoudas.bringsjord.clima.crc.pdf"
  (5am:is-true (prove '( (not (knows a now P))
                        (knows a now (implies (not Q) P))
                        (implies (not Q) (Knows a now (not Q))))
                      'Q)))

(5am:test dt13
  "testing dr3"
  (5am:is-true 
   (prove (list '(common now P)) 'P)))
