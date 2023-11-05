#lang racket/base

(require racket/contract
         racket/match)

(struct expval () #:transparent)

(struct num-val expval (x) #:transparent)
(struct bool-val expval (x) #:transparent)

(define (expval->num x)
  (match x
    [(num-val x) x]
    [(bool-val x)
     (error (format "Expected integer but got boolean ~a" x))]))

(define (expval->bool x)
  (match x
    [(bool-val x) x]
    [(num-val x)
     (error (format "Expected boolean but got integer ~a" x))]))

(provide
 expval?
 (contract-out
  [expval->num (-> expval? integer?)]
  [expval->bool (-> expval? boolean?)]
  [struct num-val ((x integer?))]
  [struct bool-val ((x boolean?))]))
