;; Name: Naveen Sundar G.
;; Email: naveensundarg@gmail.com
;; Date Created: August 4, 2014
;; Reference: http://kryten.mm.rpi.edu/arkoudas.bringsjord.clima.crc.pdf

(in-package :shadowprover)
(5am:def-suite wise-men)
(5am:in-suite wise-men)



(5am:test n=2
  (5am:is-true
   (prove '((Common now (not (knows a1 now (Marked a1))))
            (Common now (implies (not (Marked a2)) (Marked a1)))
            (Common now (implies (not (Marked a1)) (Marked a2)))
            (Common now (implies (not (Marked a2)) (Knows a1 now (not (Marked a2))))))
          '(Marked a2))))



(5am:test n=3
  (5am:is-true
   (prove '((Common now (not (knows (Marked a1) now M1)))
            (Common now (not (knows a2 now (Marked a2))))
            (Common now (implies (not (or (Marked a2) (Marked a3))) M1))
            (Common now (implies (not (or (Marked a3) M1)) (Marked a2)))
            (Common now (implies (not (or M1 (Marked a2))) (Marked a3)))
            (Common now (implies (not (or (Marked a2) (Marked a3)))
                         (knows (Marked a1) now (not (or (Marked a2) (Marked a3))))))
            (Common now (implies (not (or (Marked a3) M1))
                         (knows a2 now (not (or (Marked a3) M1)))))
            (Common now (implies (not (Marked a3)) (Knows a2 now (not (Marked a3))))))
          '(or (Marked a2) (Marked a3)))))
