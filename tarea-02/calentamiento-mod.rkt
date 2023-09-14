#lang racket

(require (for-syntax racket/string))

(begin-for-syntax
  (when (file-exists? "calentamiento-strp.rkt")
    (delete-file "calentamiento-strp.rkt"))
  (with-input-from-file "calentamiento.rkt"
    (lambda ()
      (with-output-to-file "calentamiento-strp.rkt"
        (lambda ()
          (let loop ([line (read-line)])
            (cond [(eof-object? line)]
                  [(string-prefix? line "#lang")
                   (loop (read-line))]
                  [else
                   (printf "~A~%" line)
                   (loop (read-line))])))))))

(include "calentamiento-strp.rkt")

(provide (all-defined-out))
