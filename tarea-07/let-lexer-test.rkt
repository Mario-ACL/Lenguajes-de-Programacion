#lang racket

(require rackunit
         rackunit/text-ui
         "let-tokens.rkt"
         "let-lexer.rkt")

(define (strip-pos token)
  (match token
    [(pos-token x _ _) x]
    [_ token]))

;;; tokenize : string? -> (list-of token?)
(define (tokenize input)
  (map strip-pos
       (stream->list
        (lex-let (open-input-string input)))))

(define lexer-tests
  (test-suite
   "Test suite for the LET lexer"
   
   (test-case "empty program"
     (check-equal? (tokenize "") empty))

   (test-case "negative integers and diff"
     (check-equal?
      (tokenize "-12-(-1,-1)")
      (list (int-token -12)
            (diff-token)
            (open-paren-token)
            (int-token -1)
            (comma-token)
            (int-token -1)
            (close-paren-token))))

   (test-case "example 1"
     (check-equal?
      (tokenize "-(55, -(x, 11))")
      (list (diff-token)
            (open-paren-token)
            (int-token 55)
            (comma-token)
            (diff-token)
            (open-paren-token)
            (id-token 'x)
            (comma-token)
            (int-token 11)
            (close-paren-token)
            (close-paren-token))))

   (test-case "example 1p"
     (check-equal?
      (tokenize "+(55, +(x, 11))")
      (list (plus-token)
            (open-paren-token)
            (int-token 55)
            (comma-token)
            (plus-token)
            (open-paren-token)
            (id-token 'x)
            (comma-token)
            (int-token 11)
            (close-paren-token)
            (close-paren-token))))

   (test-case "example 2"
     (check-equal?
      (tokenize "-(-(x, 3), -(v, i))")
      (list
       (diff-token)
       (open-paren-token)
       (diff-token)
       (open-paren-token)
       (id-token 'x)
       (comma-token)
       (int-token 3)
       (close-paren-token)
       (comma-token)
       (diff-token)
       (open-paren-token)
       (id-token 'v)
       (comma-token)
       (id-token 'i)
       (close-paren-token)
       (close-paren-token))))

   (test-case "example 3"
     (check-equal?
      (tokenize "if zero?(-(x, 11)) then -(y, 2) else -(y, 4)")
      (list
       (if-token)
       (zero?-token)
       (open-paren-token)
       (diff-token)
       (open-paren-token)
       (id-token 'x)
       (comma-token)
       (int-token 11)
       (close-paren-token)
       (close-paren-token)
       (then-token)
       (diff-token)
       (open-paren-token)
       (id-token 'y)
       (comma-token)
       (int-token 2)
       (close-paren-token)
       (else-token)
       (diff-token)
       (open-paren-token)
       (id-token 'y)
       (comma-token)
       (int-token 4)
       (close-paren-token))))

   (test-case "example 4"
     (check-equal?
      (tokenize "let x = 5\nin -(x,3)")
      (list
       (let-token)
       (id-token 'x)
       (equals-token)
       (int-token 5)
       (in-token)
       (diff-token)
       (open-paren-token)
       (id-token 'x)
       (comma-token)
       (int-token 3)
       (close-paren-token))))

   (test-case "example 5"
     (check-equal?
      (tokenize "
let z = 5
in let x = 3
   in let y = -(x,1)     # aqui x vale 3
      in let x = 4
         in -(z, -(x,y)) # aqui x vale 4")
      (list
       (let-token)
       (id-token 'z)
       (equals-token)
       (int-token 5)
       (in-token)
       (let-token)
       (id-token 'x)
       (equals-token)
       (int-token 3)
       (in-token)
       (let-token)
       (id-token 'y)
       (equals-token)
       (diff-token)
       (open-paren-token)
       (id-token 'x)
       (comma-token)
       (int-token 1)
       (close-paren-token)
       (in-token)
       (let-token)
       (id-token 'x)
       (equals-token)
       (int-token 4)
       (in-token)
       (diff-token)
       (open-paren-token)
       (id-token 'z)
       (comma-token)
       (diff-token)
       (open-paren-token)
       (id-token 'x)
       (comma-token)
       (id-token 'y)
       (close-paren-token)
       (close-paren-token))))

   (test-case "example 6"
     (check-equal?
      (tokenize "
let x = 7
in let y = 2
   in let y = let x = -(x,1)
              in -(x,y)
      in -(-(x,8), y)")
      (list
       (let-token)
       (id-token 'x)
       (equals-token)
       (int-token 7)
       (in-token)
       (let-token)
       (id-token 'y)
       (equals-token)
       (int-token 2)
       (in-token)
       (let-token)
       (id-token 'y)
       (equals-token)
       (let-token)
       (id-token 'x)
       (equals-token)
       (diff-token)
       (open-paren-token)
       (id-token 'x)
       (comma-token)
       (int-token 1)
       (close-paren-token)
       (in-token)
       (diff-token)
       (open-paren-token)
       (id-token 'x)
       (comma-token)
       (id-token 'y)
       (close-paren-token)
       (in-token)
       (diff-token)
       (open-paren-token)
       (diff-token)
       (open-paren-token)
       (id-token 'x)
       (comma-token)
       (int-token 8)
       (close-paren-token)
       (comma-token)
       (id-token 'y)
       (close-paren-token))))

(test-case "example minus"
     (check-equal?
      (tokenize "minus(-(minus(5),9))")
      (list (minus-token)
            (open-paren-token)
            (diff-token)
            (open-paren-token)
            (minus-token)
            (open-paren-token)
            (int-token 5)
            (close-paren-token)
            (comma-token)
            (int-token 9)
            (close-paren-token)
            (close-paren-token))))

(test-case "example mult"
     (check-equal?
      (tokenize "*(*(5,9),2)")
      (list (mult-token)
            (open-paren-token)
            (mult-token)
            (open-paren-token)
            (int-token 5)
            (comma-token)
            (int-token 9)
            (close-paren-token)
            (comma-token)
            (int-token 2)
            (close-paren-token))))))

(run-tests lexer-tests 'verbose)
