#lang racket
(define open-paren-regex #rx"\\(")
(define close-paren-regex #rx"\\)")
(define define-regex #rx"\\define")
(define sum-regex #rx"\\+")
(define mult-regex #rx"\\*")
(define identifier-regex #rx"[xyz][xyz1234567890]*")
(define number-regex #rx"[1234567890][1234567890]*")

(provide open-paren-regex
         close-paren-regex
         define-regex
         sum-regex
         mult-regex
         identifier-regex
         number-regex)