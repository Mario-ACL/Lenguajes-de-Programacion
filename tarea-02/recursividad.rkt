#lang racket
;; Program layout:
; number.- ;current exercise from Tarea 2
; program
; (example) to use with program


;1.-
(define (countdown x)
  (if (< x 0)
      '()
      (cons x (countdown (- x 1)))));Recursion until x < 0
;(countdown 5)


;2.-
(define (insertL x y L)
  (if (null? L)
      '()
      (if(eqv? (car L) x) ;compare current 'L' symbol with 'x' symbol
         (cons y (cons x (insertL x y (cdr L)))) ;if #t we insert 'y' and use cons with next recursion 
         (cons (car L) (insertL x y (cdr L))))));if #f we use cons with current symbol in 'L' and recursion of next symbol in 'L'
;(insertL 'x 'y '(x z z x y x))

;3.-
(define (remv-1st x L)
  (if (null? L)
      '()
      (if (eqv? (car L) x)
         (cdr L)
         (cons (car L)(remv-1st x (cdr L))))))
;(remv-1st 'x '(x y z x))
;(remv-1st 'y '(x y z y x))
;(remv-1st 'z '(a b c))

;4.-
(define (map p ls)
  (if (null? ls)
      '()
      (if (null? ls)
          (p (car ls))
          (cons (p (car ls)) (map p (cdr ls))))))
;(map sub1 '(1 2 3 4))


;5.-
(define (filter pred ls)
  (if (null? ls)
      '()
      (cond
        [(and (pred (car ls)) (null? (cdr ls))) (cons (car ls) empty)]
        [(pred (car ls)) (cons (car ls) (filter pred (cdr ls)))]
        [(null? (cdr ls)) (empty)]
        [else (filter pred (cdr ls))])))
;(filter even? '(1 2 3 4 5 6))

;6.-
(define (zip ls1 ls2)
  (if (or (null? ls1)(null? ls2))
      '()
      (cons (cons (car ls1) (car ls2)) (zip (cdr ls1) (cdr ls2)))))
;(zip '(1 2 3) '(a b c))
;(zip '(1 2 3 4 5 6) '(a b c))
;(zip '(1 2 3) '(a b c d e f))

;7.-
(define (list-index-ofv x ls)
  (if (null? ls)
      '()
      (if(eqv? (car ls) x)
         0
         (+ 1 (list-index-ofv x (cdr ls))))))
;(list-index-ofv 'x '(x y z x x))
;(list-index-ofv 'x '(y z x x))

;8.-
(define (append ls1 ls2)
  (if (null? ls1)
        ls2
        (cons (car ls1) (append (cdr ls1) ls2))))
;(append '(42 120) '(1 2 3))
;(append '(a b c) '(cat dog))

;9.-
(define (reverse ls)
  (if (null? ls)
    '()
    (append (reverse (cdr ls))(cons (car ls) empty)))) ;using self-defined append woohoo
;(reverse '(a 3 x))

;10.-
(define (repeat ls rept)
  (if (zero? rept)
      '()
      (append ls (repeat ls (sub1 rept))))) ;using self-defined append woohoo
;(repeat '(4 8 11) 4)

;11.-
(define (same-lists* x y)
  (equal? x y))
;(same-lists* '() '())
;(same-lists* '(1 2 3 4 5) '(1 2 3 4 5))
;(same-lists* '(1 2 3 4) '(1 2 3 4 5))
;(same-lists* '(a (b c) d) '(a (b) c d))
; NON-RECURSIVE, NEEDS TO BE REDONE

;12.-
(equal? '((w x) y (z))
        '((w . (x . ())) . (y . ((z . ()) . ()))))

;13.-
#| Cool function I made but the assignment is for an inverse binary :(
(define (binary->natural x)
  (if (null? x)
      0
      (+(*(car x)(expt 2 (- (length x) 1)))(binary->natural (cdr x)))))
|#
(define (binary->natural x)
  (if (null? x)
      0
      (+(car x)(* 2 (binary->natural (cdr x))))))
#|
(binary->natural '())
(binary->natural '(0 0 1))
(binary->natural '(0 0 1 1))
(binary->natural '(1 1 1 1))
(binary->natural '(1 0 1 0 1))
(binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1))
|#

;14.-
(define (div x y)
  (cond
    [(eqv? (zero? (modulo x y)) #f) "Oh no"]
    [(zero? x) 0]
    [else (+ 1 (div (- x y) y))]))
;(div 25 5)
;(div 36 6)

;15.-
(define (append-map x ls)
  (if (null? ls)
      '()
      (append (x(car ls)) (append-map x (cdr ls)))))
;(append-map countdown (countdown 5))

;16.-
(define (set-difference ls1 ls2)
  (cond
    [(or (null? ls1)(null? ls2)) empty]
    [(eqv? (index-of ls2 (car ls1)) #f) (cons (car ls1) (set-difference (cdr ls1) ls2))]
    [else (set-difference (cdr ls1) ls2)]))
;(set-difference '(1 2 3 4 5) '(2 6 4 8))

;17.-
(define (foldr func acum ls)
  (if (null? ls)
      acum
      (func (car ls) (foldr func acum (cdr ls)))))
;(foldr cons '() '(1 2 3 4))
;(foldr + 0 '(1 2 3 4))
;(foldr * 1 '(1 2 3 4))

;18.-
(define (powerset s)
  (if (null? s)
      (list (list)) ; or '(())
      (let ((x(first s)); first element
            (xs (rest s))) ; rest
        (let ((ss (powerset xs)))
          (append ss (map (lambda (s) (cons x s)) ;using self-defined append woohoo
                          ss))))))
;(powerset '(1 2 3))
;Done in class

;19.-
(define (cartesian-product ls)
  (cond
    ((null? ls) '(()))
    ((null? (cdr ls))(map list (car ls)))
    (else
     (let* ((ls1 (car ls))
            (ls2 (cdr ls))
            (rest-cartesian-product (cartesian-product ls2)))
       (append-map(lambda (x) (map (lambda (y) (cons x y)) rest-cartesian-product)) ls1)))))
;(cartesian-product '((5 4) (3 2 1)))

;20.-
;insert
(define (insertL-fr x y L)
  (define (insert-x z elem)
    (if (eqv? x z)
        (cons y (cons z elem))
        (cons z elem)))
  (foldr insert-x '() L))
;(insertL-fr 'x 'y '(x z z x y x))

;filter
(define (filter-fr pred L)
  (define (filter-x act ls)
    (if (pred act)
        (cons act ls)
        ls))
  (foldr filter-x '() L))
;(filter-fr even? '(1 2 3 4 5 6))

;map
(define (map-fr func L)
  (define (map-x act ls)
    (cons (func act) ls))
  (foldr map-x '() L))
;(map sub1 '(1 2 3 4))

;append
(define (append-fr ls1 ls2)
  (foldr cons ls2 ls1))
;(append '(42 120) '(1 2 3))
;(append '(a b c) '(cat dog))

;reverse
(define (reverse-fr L)
  (define (reverse-x x ls)
    (append ls (list x)))
  (foldr reverse-x '() L))
;(reverse '(a 3 x))

;binary->natural
(define (binary->natural-fr L)
  (define (binary->natural-x x ls)
    (+ x (* 2 ls)))
  (foldr binary->natural-x 0 L))
#|
(binary->natural '())
(binary->natural '(0 0 1))
(binary->natural '(0 0 1 1))
(binary->natural '(1 1 1 1))
(binary->natural '(1 0 1 0 1))
(binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1))
|#

;append-map
(define (append-map-fr func L)
  (define (append-map-x x ls)
    (append (func x) ls))
  (foldr append-map-x '() L))
;(append-map countdown (countdown 5))

;set-difference
(define (set-difference-fr ls1 ls2)
  (define (set-difference-x x ls)
    (if (member x ls2)
        ls
        (cons x ls)))
  (foldr set-difference-x '() ls1))
;(set-difference '(1 2 3 4 5) '(2 6 4 8))

;powerset
(define (powerset-fr s)
  (define (powerset-x x ps)
    (append (map (lambda (comb) (cons x comb)) ps) ps))
  
  (foldr powerset-x (list '()) s))
;(powerset-fr '(3 2 1))

;21.-
(define snowball
  (letrec
      ((odd-case
        (lambda (fix-odd)
          (lambda (x)
            (cond
              ((and (exact-integer? x) (positive? x) (odd? x)) (snowball
                                                                (add1 (* x 3))))
                                                                                (else (fix-odd x))))))
       (even-case
        (lambda (fix-even)
          (lambda (x)
            (cond
              ((and (exact-integer? x) (positive? x) (even? x)) (snowball
                                                                 (/ x 2)))
              (else (fix-even x))))))
       (one-case
        (lambda (fix-one)
          (lambda (x)
            (cond
              ((zero? (sub1 x)) 1)
              (else (fix-one x))))))
       (base
        (lambda (x)
          (error 'error "Invalid value ~s~n" x)))) (one-case (even-case (odd-case base)))))
;(snowball 12)

(define (quine)
  ((λ (x) `(,x ',x)) '(λ (x) `(,x ',x))))
