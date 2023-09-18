#|
#lang stacker/smol/fun
(defvar v0 (mvec (mvec 3)(mvec 4)))
(defvar v1 (vec-ref v0 0))
(defvar v2 (vec-ref v0 1))

(deffun (pause) 0)
(vec-set! v1 0 v2)
(vec-set! v2 0 v1)
v0
(pause)
|#

#|
#lang stacker/smol/hof
(letrec [(x (lambda (z)(+ x z)))]
  (letrec [(f x)]
    (set! x 3)
    (letrec [(x 4)]
      (f 5))))
|#

#|
#lang stacker/smol/hof
(deffun (f z)
  (lambda (w) (+ w z)))
((f 3) 4)
|#

#|
#lang stacker/smol/hof
(defvar f 0)
(let [(ctr 0)]
  (set! f (lambda () (set! ctr (+ ctr 1)) ctr))
  (f))
(f)
|#









