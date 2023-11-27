#lang racket/base

(require racket/list
         racket/stream
         racket/match
         racket/function
         "let-errors.rkt"
         "let-locs.rkt"
         "let-tokens.rkt"
         "let-ast.rkt")

(define (raise-parse-error message tokens)
  (if (stream-empty? tokens)
      (read-error
       (format "Expected ~a but got end of input" message)
       #f #f)
      (match (stream-first tokens)
        [(pos-token token beg end)
         (read-error
          (format "Expected ~a but got ~a"
                  message
                  (match token
                    [(int-token num) (format "integer ~a" num)]
                    [(id-token name) (format "identifier ~a" name)]
                    [(open-paren-token) "open parenthesis"]
                    [(close-paren-token) "close parenthesis"]
                    [(zero?-token) "zero? operator"]
                    [(diff-token) "- operator"]
                    [(comma-token) "a comma"]
                    [(if-token) "if keyword"]
                    [(then-token) "then keyword"]
                    [(else-token) "else keyword"]
                    [(let-token) "let keyword"]
                    [(equals-token) "binding operator"]
                    [(in-token) "in keyword"]
                    [(minus-token) "minus operator"]
                    [(plus-token) "+ operator"]
                    [(cons-token) "cons operator"]
                    [(car-token) "car operator"]
                    [(cdr-token) "cdr operator"]
                    [(null?-token) "null? operator"]
                    [(emptylist-token) "emptylist value"]
                    [(list-token) "list operator"]
                    [(print-token) "print keyword"]
                    [_ "unexpected token"]))
          beg end)])))

(define (parse-let tokens)
  (define-values (program tokens*)
    ((guard parse-program "a program")
     tokens))
  program)

(define (parse-program tokens)
  ((parse/seq a-program
              parse-expression
              (guard expect-empty "end of program"))
   tokens))

(define (parse-expression tokens)
  ((parse/alt (parse/seq const-exp
                         (expect-some int-token? int-token-num))
              (parse/seq diff-exp
                         (expect-sugar diff-token?)
                         (guard (expect-sugar open-paren-token?) "open parenthesis")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar comma-token?) "a comma")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar close-paren-token?) "close parenthesis"))
              (parse/seq minus-exp
                         (expect-sugar minus-token?)
                         (guard (expect-sugar open-paren-token?) "open parenthesis")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar close-paren-token?) "close parenthesis"))
              (parse/seq plus-exp
                         (expect-sugar plus-token?)
                         (guard (expect-sugar open-paren-token?) "open parenthesis")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar comma-token?) "a comma")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar close-paren-token?) "close parenthesis"))
              (parse/seq mult-exp
                         (expect-sugar mult-token?)
                         (guard (expect-sugar open-paren-token?) "open parenthesis")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar comma-token?) "a comma")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar close-paren-token?) "close parenthesis"))
              (parse/seq div-exp
                         (expect-sugar div-token?)
                         (guard (expect-sugar open-paren-token?) "open parenthesis")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar comma-token?) "a comma")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar close-paren-token?) "close parenthesis"))
              (parse/seq zero?-exp
                         (expect-sugar zero?-token?)
                         (guard (expect-sugar open-paren-token?) "open parenthesis")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar close-paren-token?) "close parenthesis"))
              (parse/seq if-exp
                         (expect-sugar if-token?)
                         (guard parse-expression "an expression")
                         (guard (expect-sugar then-token?) "then keyword")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar else-token?) "else keyword")
                         (guard parse-expression "an expression"))
              (parse/seq var-exp
                         (expect-some id-token? id-token-name))
              (parse/seq let-exp
                         (expect-sugar let-token?)
                         (guard (expect-some id-token? id-token-name) "an identifier")
                         (guard (expect-sugar equals-token?) "binding operator")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar in-token?) "in keyword")
                         (guard parse-expression "an expression"))
              (parse/seq cons-exp
                         (expect-sugar cons-token?)
                         (guard (expect-sugar open-paren-token?) "open parenthesis")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar comma-token?) "a comma")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar close-paren-token?) "close parenthesis"))
              (parse/seq car-exp
                         (expect-sugar car-token?)
                         (guard (expect-sugar open-paren-token?) "open parenthesis")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar close-paren-token?) "close parenthesis"))
              (parse/seq cdr-exp
                         (expect-sugar cdr-token?)
                         (guard (expect-sugar open-paren-token?) "open parenthesis")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar close-paren-token?) "close parenthesis"))
              (parse/seq null?-exp
                         (expect-sugar null?-token?)
                         (guard (expect-sugar open-paren-token?) "open parenthesis")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar close-paren-token?) "close parenthesis"))
              (parse/seq emptylist-exp
                         (expect-sugar emptylist-token?))
              (parse/seq print-exp
                         (expect-sugar print-token?)
                         (guard (expect-sugar open-paren-token?) "open parenthesis")
                         (guard parse-expression "an expression")
                         (guard (expect-sugar close-paren-token?) "close parenthesis")))
   
   tokens))

(define (parse/alt . parsers)
  (define (try-parsers tokens parsers)
    (cond [(null? parsers)
           (values #f tokens)]
          [else
           (define-values (arg tokens*) ((first parsers) tokens))
           (cond [(not arg)
                  (try-parsers tokens (rest parsers))]
                 [else
                  (values arg tokens*)])]))
  (lambda (tokens)
    (try-parsers tokens parsers)))

(define (parse/seq construct . parsers)
  (define (match-parsers tokens parsers vals)
    (cond [(null? parsers)
           (values (apply construct (reverse vals)) tokens)]
          [else
           (define-values (val tokens*) ((first parsers) tokens))
           (cond [(not val)
                  (values #f tokens*)]
                 [(eq? val 'ignore)
                  (match-parsers tokens* (rest parsers) vals)]
                 [else
                  (match-parsers tokens* (rest parsers) (cons val vals))])]))
  (lambda (tokens)
    (match-parsers tokens parsers null)))

(define (guard parser message)
  (lambda (tokens)
    (define-values (val tokens*) (parser tokens))
    (if (not val)
        (raise-parse-error message tokens*)
        (values val tokens*))))

(define (expect-empty tokens)
  (if (stream-empty? tokens)
      (values 'ignore tokens)
      (values #f tokens)))

(define (expect-some token? select)
  (lambda (tokens)
    (if (stream-empty? tokens)
        (values #f tokens)
        (match (stream-first tokens)
          [(pos-token token beg end)
           (if (token? token)
               (values (select token) (stream-rest tokens))
               (values #f tokens))]))))

(define (expect-sugar token?)
  (expect-some token? (const 'ignore)))

(provide parse-let)
