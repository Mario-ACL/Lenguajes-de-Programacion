#lang racket
(require rackunit
         rackunit/text-ui
         "regex.rkt")
(define regex-tests
  (test-suite
   "Pruebas para regex.rkt"
   (test-case "open-paren-regex"
              (check-equal? (regexp-match open-paren-regex "(Hola)") '("(")))
   (test-case "close-paren-regex"
              (check-equal? (regexp-match close-paren-regex "(Wow)") '(")")))
   (test-case "define-regex"
              (check-equal? (regexp-match define-regex "define (funcion x))") '("define")))
   (test-case "sum-regex"
              (check-equal? (regexp-match sum-regex "(+ 5 6)") '("+")))
   (test-case "mult-regex"
              (check-equal? (regexp-match mult-regex "(* 5 6)") '("*")))
   (test-case "identifier-regex"
              (check-equal? (regexp-match identifier-regex "(define x)") '("x")))
   (test-case "number-regex"
              (check-equal? (regexp-match number-regex "55 hola") '("55")))))
(run-tests regex-tests 'verbose)