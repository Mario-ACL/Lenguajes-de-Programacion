#lang racket/base

(require racket/contract
         "let-locs.rkt")

(struct token () #:transparent)
(struct pos-token token (val beg end) #:transparent)
(struct int-token token (num) #:transparent)
(struct id-token token (name) #:transparent)
(struct open-paren-token token () #:transparent)
(struct close-paren-token token () #:transparent)
(struct zero?-token token () #:transparent)
(struct diff-token token () #:transparent)
(struct comma-token token () #:transparent)
(struct if-token token () #:transparent)
(struct then-token token () #:transparent)
(struct else-token token () #:transparent)
(struct let-token token () #:transparent)
(struct equals-token token () #:transparent)
(struct in-token token () #:transparent)
(struct minus-token token () #:transparent)
(struct plus-token token () #:transparent)
(struct mult-token token () #:transparent)
(struct div-token token () #:transparent)
(struct cons-token token () #:transparent)
(struct car-token token () #:transparent)
(struct cdr-token token () #:transparent)
(struct null?-token token () #:transparent)
(struct emptylist-token token () #:transparent)
(struct list-token token () #:transparent)
(struct print-token token () #:transparent)

(provide
 token?
 (contract-out
  [struct pos-token ((val token?) (beg pos?) (end pos?))]
  [struct int-token ((num integer?))]
  [struct id-token ((name symbol?))]
  [struct open-paren-token ()]
  [struct close-paren-token ()]
  [struct zero?-token ()]
  [struct diff-token ()]
  [struct comma-token ()]
  [struct if-token ()]
  [struct then-token ()]
  [struct else-token ()]
  [struct let-token ()]
  [struct equals-token ()]
  [struct in-token ()]
  [struct minus-token ()]
  [struct plus-token ()]
  [struct mult-token ()]
  [struct div-token ()]
  [struct cons-token ()]
  [struct car-token ()]
  [struct cdr-token ()]
  [struct null?-token ()]
  [struct emptylist-token ()]
  [struct list-token ()]
  [struct print-token ()]
  ))
