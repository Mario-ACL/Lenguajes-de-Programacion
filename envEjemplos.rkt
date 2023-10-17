#lang racket

(define (empty-env)
  (lambda (var)
    (error "list no definida")))

(define (apply-env env var)
  (env var))

(define (extend-env var val env)
  (lambda (var*)
    (if(eq? var var*)
       val
       (env var*))))

(define (eval-prog prog env)
  (cond
    [(equal? env (empty-env)) prog]
    [(integer? prog) prog]
    [(and (eq? (eval-def (first prog) env) env) (integer? (eval-prog (second prog) env))) prog]))

(define (eval-def def env)
  (cond
    [(and (list? def) (= (length def) 3) (eq? '(define) (first def)) (symbol? (second def))
          (integer? (third def))) env]
    [else (error "No definido correctamente")]))

(define (eval-exp exp env)
  (cond
    [(integer? exp) exp]
    [(symbol? exp) (apply-env env exp)]
    [(and (list? exp)
          (=(length exp)3)
          (eq?(first exp)'+))
     (let*([exp1(second exp)]
          [exp2(third exp)]
          [x1 (eval-exp exp1 env)]
          [x2 (eval-exp exp2 env)])
       (+ x1 x2))]
    [(and (list? exp)
          (=(length exp)3)
          (eq?(first exp)'*))
     (let*([exp1(second exp)]
          [exp2(third exp)]
          [x1 (eval-exp exp1 env)]
          [x2 (eval-exp exp2 env)])
       (* x1 x2))]))


#|
(define (envi y)
(eval-exp 'x (apply-env (extend-env 'x 5 (empty-env))'x))

(define (empty-env)
  '())

(define (apply-env env var)
  (if (null? env)
      (error "list no definida")
      (if(eq?(car (car env)) var)
         (cdr (car env))
         (apply-env (cdr env) var))))

(define (extend-env var val env)
  (cons (cons var val) env))
|#