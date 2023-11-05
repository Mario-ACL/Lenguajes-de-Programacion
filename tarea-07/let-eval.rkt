#lang racket/base

(require racket/contract
         racket/match
         "let-ast.rkt"
         "let-vals.rkt"
         "let-env.rkt")

(define (value-of-program pgm)
  (match pgm
    [(a-program exp1)
     (value-of exp1 (init-env))]
    [_
     (error (format "Expected program but got ~a" pgm))]))

(define (value-of exp env)
  (match exp
    [(const-exp num)
     (num-val num)]
    [(var-exp var)
     (apply-env env var)]
    [(diff-exp exp1 exp2)
     (num-val (- (expval->num (value-of exp1 env))
                 (expval->num (value-of exp2 env))))]
    [(minus-exp exp1)
     (num-val (- (expval->num (value-of exp1 env))))]
    [(plus-exp exp1 exp2)
     (num-val (+ (expval->num (value-of exp1 env))
                 (expval->num (value-of exp2 env))))]
    [(mult-exp exp1 exp2)
     (num-val (* (expval->num (value-of exp1 env))
                 (expval->num (value-of exp2 env))))];agregar div
    [(zero?-exp exp1)
     (bool-val (zero? (expval->num (value-of exp1 env))))]
    [(if-exp exp1 exp2 exp3)
     (if (expval->bool (value-of exp1 env))
         (value-of exp2 env)
         (value-of exp3 env))]
    [(let-exp var exp1 body)
     (value-of body (extend-env var (value-of exp1 env) env))]
    [_
     (error (format "Expected expression but got ~a" exp))]))

(provide
 (contract-out
  [value-of-program (-> a-program? expval?)]
  [value-of (-> expression? environment? expval?)]))
