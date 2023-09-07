#lang racket

(define pi 3.14)
(define (area-circle r)
  (* pi (* r r)))
;(area-circle 5)

(define (circle-properties r)
  (define circunf(* 2 (* r pi)))
  (list (area-circle r) circunf ))
;(circle-properties 5)

(define (rectangle-properties x)
  (define a (list-ref x 0))
  (define b (list-ref x 1))
  (list (* a b) (+ (+ a a) (+ b b))))
;(rectangle-properties '(2 4))

(define (find-needle x)
  (define (index-of-needle y)
    (for/first ([i (in-range (length y))]
                [elem (in-list y)]
                #:when (equal? elem 'needle))
      i))
  (if (boolean? (index-of-needle x)) -1 (index-of-needle x)))
;(find-needle '(hay needle hay))
;(find-needle '(hay hay hay))

(define (abs x)
  (if ( positive? x) x (* x -1)))
;(abs 3)
;(abs -2)

(define (listSum x)
  (map (lambda (number)
         (+ 1 number))
       x))
;(listSum '(1 2 3))

;map even funciona por su cuenta
;(map even? '(1 2 3 4 5 6))

(define another-add
  (lambda (n m)
    (cond
      [(zero? n) m]
      [else (add1 (another-add m (sub1 n)))])))
;end of program