#lang racket

(require (for-syntax racket/string))

(begin-for-syntax
  (when (file-exists? "recursividad-strp.rkt")
    (delete-file "recursividad-strp.rkt"))
  (with-input-from-file "recursividad.rkt"
    (lambda ()
      (with-output-to-file "recursividad-strp.rkt"
        (lambda ()
          (let loop ([line (read-line)])
            (cond [(eof-object? line)]
                  [(string-prefix? line "#lang")
                   (loop (read-line))]
                  [else
                   (printf "~A~%" line)
                   (loop (read-line))])))))))

(include "recursividad-strp.rkt")

(provide (all-defined-out))
