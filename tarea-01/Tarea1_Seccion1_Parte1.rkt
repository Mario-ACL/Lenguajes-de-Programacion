#lang stacker/smol/fun

(deffun (pause) 0)

(deffun (sum y)
  (+ (pause) y))
(deffun (sum2 x)
  (+ (sum 6) x))

(sum2 3)