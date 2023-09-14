#lang racket

(require rackunit
         rackunit/text-ui
         racket/sandbox
         (prefix-in hacker: "calentamiento-mod.rkt"))

(define-syntax (if-defined stx)
  (syntax-case stx ()
    [(_ id iftrue iffalse)
     (let ([where (identifier-binding #'id)])
       (if where #'iftrue #'iffalse))]))

(define-syntax with-guard
  (syntax-rules ()
    [(_ id iftrue ...)
     (if-defined id
                 (with-handlers ([exn:fail:resource?
                                  (lambda (err)
                                    (fail "plomo detectado"))])
                   (with-limits 2 2
                     iftrue ...))
                 (fail "sin definición"))]))

(define calentamiento
  (test-suite
   "Pruebas para calentamiento.rkt"

   (test-case "pi"
     (with-guard hacker:pi
       (check-within hacker:pi 3.14 0.000001)))
   
   (test-case "area-circle"
     (with-guard hacker:area-circle
       (check-eqv? (hacker:area-circle 5) 78.5)))
   
   (test-case "circle-properties"
     (with-guard hacker:circle-properties
       (check-within (hacker:circle-properties 5) '(78.5 31.4) 0.000001)))
   
   (test-case "rectangle-properties"
     (with-guard hacker:rectangle-properties
       (check-equal? (hacker:rectangle-properties '(2 4)) '(8 12))))
   
   (test-case "find-needle"
     (with-guard hacker:find-needle
       (check-eqv? (hacker:find-needle '(hay needle hay)) 1)
       (check-eqv? (hacker:find-needle '(hay hay hay)) -1)))
   
   (test-case "abs"
     (with-guard hacker:abs
       (check-eqv? (abs 3) 3)
       (check-eqv? (abs -2) 2)))
   
   (test-case "inclis1"
     (with-guard hacker:inclis1
       (check-equal? (hacker:inclis1 '(1 2 3)) '(2 3 4))))
   
   (test-case "even?"
     (with-guard hacker:even?
      (check-equal? (map even? '(1 2 3 4 5 6)) '(#f #t #f #t #f #t))))

   (test-case "another-add"
     (with-guard hacker:another-add
       (check-equal? (hacker:another-add 0 0) 0)
       (check-equal? (hacker:another-add 10 0) 10)
       (check-equal? (hacker:another-add 0 10) 10)
       (check-equal? (hacker:another-add 42 666) 708)
       (check-equal? (hacker:another-add 666 42) 708)))))

(run-tests calentamiento 'verbose)
