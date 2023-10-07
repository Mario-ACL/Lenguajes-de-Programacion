#lang racket

(require rackunit
         rackunit/text-ui
         "lexer.rkt")

(define lexer-tests
  (test-suite
   "Pruebas para lexer.rkt"
   (test-case "Lexer"
              (check-equal?
               (stream->list (lex-from-string "("))
                (list (token 'open-paren #f 1 0)))
              
              (check-equal?
               (stream->list (lex-from-string ")"))
                (list (token 'close-paren #f 1 0)))
              
              (check-equal?
               (stream->list (lex-from-string "+"))
                (list (token 'binop '+ 1 0)))
              
              #|Returns error and thats the expected way
              (check-equal?
               (stream->list (lex-from-string "-"))
                (list (token 'binop '- 1 0)))
              |#
              
              (check-equal?
               (stream->list (lex-from-string "*"))
                (list (token 'binop '* 1 0)))

              (check-equal?
               (stream->list (lex-from-string ""))
                empty)

              (check-equal?
               (stream->list (lex-from-string "-5"))
                (list (token 'number -5 1 0)))

              (check-equal?
               (stream->list (lex-from-string "5"))
                (list (token 'number 5 1 0)))

              (check-equal?
               (stream->list (lex-from-string "(define x5 34)"))
                (list
                 (token 'open-paren #f 1 0)
                 (token 'define #f 1 1)
                 (token 'identifier 'x5 1 8)
                 (token 'number 34 1 11)
                 (token 'close-paren #f 1 13)))

              (check-equal?
               (stream->list (lex-from-string "zyx"))
                (list (token 'identifier 'zyx 1 0)))

              (check-equal?
               (stream->list (lex-from-string "(define y99 -34)"))
               (list
                (token 'open-paren #f 1 0)
                (token 'define #f 1 1)
                (token 'identifier 'y99 1 8)
                (token 'number -34 1 12)
                (token 'close-paren #f 1 15)))
              
              (check-equal?
               (stream->list (lex-from-string "(+ 5 (* 3 4))"))
               (list
                (token 'open-paren #f 1 0)
                (token 'binop '+ 1 1)
                (token 'number 5 1 3)
                (token 'open-paren #f 1 5)
                (token 'binop '* 1 6)
                (token 'number 3 1 8)
                (token 'number 4 1 10)
                (token 'close-paren #f 1 11)
                (token 'close-paren #f 1 12))))))
               
              

(run-tests lexer-tests 'verbose)
