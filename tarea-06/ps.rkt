#lang racket

;given functions
(define (unit-string-list? x)
  (or (null? x)
      (and (pair? x)
           (string? (car x))
           (= (string-length (car x)) 1)
           (unit-string-list? (cdr x)))))

(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (map string (string->list s)))

(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e"
       ls))
  (apply string-append ls))

;created functions
;take l n: returns n elements from l in order (ignoring l size)
(define (take l n)
  (if(or (empty? l)(zero? n))
     empty
     (cons (first l) (take (rest l) (sub1 n)))))
;drop l n: returns l without first n elements, empty if not enough elements
(define (drop l n)
  (if(or(empty? l)(zero? n))
     l
     (drop (rest l)(sub1 n))))

;given function
(define (bundle s n)
  (cond
    [(null? s) null]
    [(or (zero? n)(negative? n)) null] ;added to cover my tests
    [else
     (cons (implode (take s n))
           (bundle (drop s n) n))]))

;created functions
(define (list->chunks l n)
  (cond
  [(null? l) null]
  [(or (zero? n)(negative? n)) null]
  [else
   (cons (take l n)
         (list->chunks (drop l n) n))]))
(define (partition s n)
  (cond
  [(null? s) null]
  [(or (zero? n)(negative? n)) null]
  [(<= (string-length s) n) (cons s empty)]
  [else
   (cons (substring s 0 n) (partition (substring s n) n))]))
  



     
(provide (all-defined-out))