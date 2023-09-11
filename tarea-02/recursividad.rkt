#lang racket
;Program layout:
;number.- ;current exercise from Tarea 2
;program
;(example) to use with program


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