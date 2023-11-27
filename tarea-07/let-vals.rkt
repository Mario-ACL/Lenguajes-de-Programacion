#lang racket/base

(require racket/contract
         racket/match)

(struct expval () #:transparent)

(struct num-val expval (x) #:transparent)
(struct bool-val expval (x) #:transparent)
(struct pair-val expval (x) #:transparent)
(struct null-val expval () #:transparent)

(define (expval->num x)
  (match x
    [(num-val x) x]
    [(bool-val x)
     (error (format "Expected integer but got boolean ~a" x))]
    [(pair-val x)
     (error (format "Expected integer but got pair ~a" x))]
    [(null-val)
     (error (format "Expected integer but got null ~a" x))]))

(define (expval->bool x)
  (match x
    [(bool-val x) x]
    [(num-val x)
     (error (format "Expected boolean but got integer ~a" x))]
    [(pair-val x)
     (error (format "Expected boolean but got pair ~a" x))]
    [(null-val)
     (error (format "Expected boolean but got null ~a" x))]))

(define (expval->pair x)
  (match 
    [(pair-val x) x]
    [(num-val x)
     (error (format "Expected pair but got integer ~a" x))]
    [(bool-val x)
     (error (format "Expected pair but got boolean ~a" x))]
    [(null-val)
     (error (format "Expected pair but got null ~a" x))]))

(define (expval->null x)
  (match x
    [(null-val) '()]
    [(num-val x)
     (error (format "Expected null but got integer ~a" x))]
    [(bool-val x)
     (error (format "Expected null but got boolean ~a" x))]
    [(pair-val x)
     (error (format "Expected null but got pair ~a" x))]))

(provide
 expval?
 (contract-out
  [expval->num (-> expval? integer?)]
  [expval->bool (-> expval? boolean?)]
  [expval->pair (-> expval? pair?)]
  [expval->null (-> expval? null?)]
  [struct num-val ((x integer?))]
  [struct bool-val ((x boolean?))]
  [struct pair-val ((x pair?))]
  [struct null-val ()]))
