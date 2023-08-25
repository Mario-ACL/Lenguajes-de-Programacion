#lang stacker/smol/state

(deffun (pause) 0)

(defvar v1 (mvec 1))
(defvar v2 (mvec 2))

(vec-set! v1 0 v2)
(vec-set! v2 0 v1)
(pause)


