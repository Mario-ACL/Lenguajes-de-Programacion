#lang racket


(require rackunit
         rackunit/text-ui
         "ps.rkt")

(define ps-tests
  (test-suite
   "Test for ps.rkt"
   (test-case "bundle"
              (check-equal? (bundle (explode "abcdefg") 3)
                            (list "abc" "def" "g"))
              (check-equal? (bundle '("a" "b") 3)
                            (list "ab"))
              (check-equal? (bundle '() 3)
                            '())
              (check-equal? (bundle (explode "abcdef") 3)
                            (list "abc" "def"))
              (check-equal? (bundle (explode "abcdef") 1)
                            (list "a" "b" "c" "d" "e" "f"))
              (check-equal? (bundle (explode "abcdef") 0) ;gave error so bundle needed specific solution
                            (list))
              (check-equal? (bundle (explode "abcdef") -1) ;gave error so bundle needed specific solution
                            (list)))
   (test-case "list->chunks"
              (check-equal? (list->chunks '("a" "b" "c" "d" "e" "f" "g") 3)
                            '(("a" "b" "c") ("d" "e" "f") ("g")))
              (check-equal? (list->chunks '("a" "b") 5)
                            '(("a" "b")))
              (check-equal? (list->chunks '() 3)
                            '())
              (check-equal? (list->chunks '("a" "b" "c" "d" "e" "f" "g") 2)
                            '(("a" "b") ("c" "d") ("e" "f") ("g")))
              (check-equal? (list->chunks '("a" "b" "c" "d" "e" "f" "g") 0)
                            '())
              (check-equal? (list->chunks '("a" "b" "c" "d" "e" "f" "g") -1)
                            '()))
   (test-case "partition"
              (check-equal? (partition "abcdefg" 3)
                            (list "abc" "def" "g"))
              (check-equal? (partition "ab" 3)
                            (list "ab"))
              (check-equal? (partition '() 3)
                            '())
              (check-equal? (partition "abcdef" 3)
                            (list "abc" "def"))
              (check-equal? (partition "abcdef" 1)
                            (list "a" "b" "c" "d" "e" "f"))
              (check-equal? (partition "abcdef" 0) ;gave error so bundle needed specific solution
                            (list))
              (check-equal? (partition "abcdef" -1) ;gave error so bundle needed specific solution
                            (list)))
   ))

(run-tests ps-tests 'verbose)