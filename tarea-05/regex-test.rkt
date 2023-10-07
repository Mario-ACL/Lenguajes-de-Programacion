#lang racket
(require rackunit
         rackunit/text-ui
         "regex.rkt")
(define regex-tests
  (test-suite
   "Pruebas para regex.rkt"
   (test-case "open-paren-regex"
              (check-true (regexp-match-exact? open-paren-regex "("))
              (check-false (regexp-match-exact? open-paren-regex ")"))
              (check-false (regexp-match open-paren-regex "Hi")))
   (test-case "close-paren-regex"
              (check-true (regexp-match-exact? close-paren-regex ")"))
              (check-false (regexp-match-exact? close-paren-regex "("))
              (check-false (regexp-match-exact? close-paren-regex "420")))
   (test-case "define-regex"
              (check-true (regexp-match-exact? define-regex "define"))
              (check-false (regexp-match-exact? define-regex "defino"))
              (check-false (regexp-match-exact? define-regex "this is wrong")))
   (test-case "sum-regex"
              (check-true (regexp-match-exact? sum-regex "+"))
              (check-false (regexp-match-exact? sum-regex "*"))
              (check-false (regexp-match-exact? sum-regex "wrong - correct")))
   (test-case "mult-regex"
              (check-true (regexp-match-exact? mult-regex "*"))
              (check-false (regexp-match-exact? mult-regex "+"))
              (check-false (regexp-match-exact? mult-regex "wrong + really - 5")))
   (test-case "identifier-regex"
              (check-true (regexp-match-exact? identifier-regex "x5"))
              (check-false (regexp-match-exact? identifier-regex "g"))
              (check-false (regexp-match-exact? identifier-regex "99No")))
   (test-case "number-regex"
              (check-true (regexp-match-exact? number-regex "55"))
              (check-false (regexp-match-exact? number-regex "a55"))
              (check-false (regexp-match-exact? number-regex "Esto no es numero")))))
(run-tests regex-tests 'verbose)