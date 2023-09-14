#lang racket

(require rackunit
         rackunit/text-ui
         racket/sandbox
         (prefix-in hacker: "recursividad-mod.rkt"))

(define-syntax (if-defined stx)
  (syntax-case stx ()
    [(_ id iftrue iffalse)
     (let ([where (identifier-binding #'id)])
       (if where #'iftrue #'iffalse))]))

(define-syntax with-guard
  (syntax-rules ()
    [(_ id iftrue ...)
     (if-defined id
                 (with-handlers ([exn:fail:resource?
                                  (lambda (err)
                                    (fail "plomo detectado"))])
                   (with-limits 2 2
                     iftrue ...))
                 (fail "sin definición"))]))

(define recursividad
  (test-suite
   "Pruebas para recursividad.rkt"

   (test-case "countdown"
     (with-guard hacker:countdown
       (check-equal? (hacker:countdown 5)
                     '(5 4 3 2 1 0))))

   (test-case "insertL"
     (with-guard hacker:insertL
       (check-equal? (hacker:insertL 'x 'y '(x z z x y x))
                     '(y x z z y x y y x))))
   
   (test-case "remv-1st"
     (with-guard hacker:remv-1st
       (check-equal? (hacker:remv-1st 'x '(x y z x))
                     '(y z x))
       (check-equal? (hacker:remv-1st 'y '(x y z y x))
                     '(x z y x))
       (check-equal? (hacker:remv-1st 'z '(a b c))
                     '(a b c))))
   
   (test-case "map"
     (with-guard hacker:map
       (check-equal? (hacker:map sub1 '(1 2 3 4))
                     '(0 1 2 3))))
   
   (test-case "filter"
     (with-guard hacker:filter
       (check-equal? (hacker:filter even? '(1 2 3 4 5 6))
                     '(2 4 6))))

   (test-case "zip"
     (with-guard hacker:zip
       (check-equal? (hacker:zip '(1 2 3) '(a b c))
                     '((1 . a) (2 . b) (3 . c)))
       (check-equal? (hacker:zip '(1 2 3 4 5 6) '(a b c))
                     '((1 . a) (2 . b) (3 . c)))
       (check-equal? (hacker:zip '(1 2 3) '(a b c d e f))
                     '((1 . a) (2 . b) (3 . c)))))

   (test-case "list-index-ofv"
     (with-guard hacker:list-index-ofv
       (check-eqv? (hacker:list-index-ofv 'x '(x y z x x)) 0)
       (check-eqv? (hacker:list-index-ofv 'x '(y z x x)) 2)))
   
   (test-case "append"
     (with-guard hacker:append
       (check-equal? (hacker:append '(42 120) '(1 2 3))
                     '(42 120 1 2 3))
       (check-equal? (hacker:append '(a b c) '(cat dog))
                     '(a b c cat dog))))
   
   (test-case "reverse"
     (with-guard hacker:reverse
       (check-equal? (hacker:reverse '(a 3 x))
                     '(x 3 a))))

   (test-case "repeat"
     (with-guard hacker:repeat
       (check-equal? (hacker:repeat '(4 8 11) 4)
                     '(4 8 11 4 8 11 4 8 11 4 8 11))))
   
   (test-case "same-lists*"
     (with-guard hacker:same-lists*
       (check-true (hacker:same-lists* '() '()))
       (check-true (hacker:same-lists* '(1 2 3 4 5) '(1 2 3 4 5)))
       (check-false (hacker:same-lists* '(1 2 3 4) '(1 2 3 4 5)))
       (check-false (hacker:same-lists* '(a (b c) d) '(a (b) c d)))
       (check-true (hacker:same-lists* '((a) b (c d) d) '((a) b (c d) d)))))
   
   (test-case "binary->natural"
     (with-guard hacker:binary->natural
       (check-eqv? (hacker:binary->natural '()) 0)
       (check-eqv? (hacker:binary->natural '(0 0 1)) 4)
       (check-eqv? (hacker:binary->natural '(0 0 1 1)) 12)
       (check-eqv? (hacker:binary->natural '(1 1 1 1)) 15)
       (check-eqv? (hacker:binary->natural '(1 0 1 0 1)) 21)
       (check-eqv? (hacker:binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191)))
   
   (test-case "div"
     (with-guard hacker:div
       (check-eqv? (hacker:div 25 5) 5)
       (check-eqv? (hacker:div 36 6) 6)))
   
   (test-case "append-map"
     (with-guard hacker:append-map
       (check-equal? (hacker:append-map hacker:countdown (hacker:countdown 5))
                     '(5 4 3 2 1 0 4 3 2 1 0 3 2 1 0 2 1 0 1 0 0))))
   
   (test-case "set-difference"
     (with-guard hacker:set-difference
       (check-equal? (list->set (hacker:set-difference '(1 2 3 4 5) '(2 6 4 8)))
                     (list->set '(1 3 5)))))
   
   (test-case "foldr"
     (with-guard hacker:foldr
       (check-equal? (hacker:foldr cons '() '(1 2 3 4))
                     '(1 2 3 4))
       (check-eqv? (hacker:foldr + 0 '(1 2 3 4))
                   10)
       (check-eqv? (hacker:foldr * 1 '(1 2 3 4))
                   24)))
   
   (test-case "powerset"
     (with-guard hacker:powerset
       (check-equal? (list->set (map list->set (hacker:powerset '(3 2 1))))
                     (list->set (map list->set '((3 2 1) (3 2) (3 1) (3) (2 1) (2) (1) ()))))
       (check-equal? (hacker:powerset '())
                     '(()))))
   
   (test-case "cartesian-product"
     (with-guard hacker:cartesian-product
       (check-equal? (list->set (hacker:cartesian-product '((5 4) (3 2 1))))
                     (list->set '((5 3) (5 2) (5 1) (4 3) (4 2) (4 1))))))

   (test-case "insertL-fr"
     (with-guard hacker:insertL-fr
       (with-guard hacker:insertL
         (check-equal? (hacker:insertL-fr 'x 'y '(x z z x y x))
                       (hacker:insertL 'x 'y '(x z z x y x))))))

   (test-case "filter-fr"
     (with-guard hacker:filter-fr
       (with-guard hacker:filter
         (check-equal? (hacker:filter-fr even? '(1 2 3 4 5 6))
                       (hacker:filter even? '(1 2 3 4 5 6))))))

   (test-case "map-fr"
     (with-guard hacker:map-fr
       (with-guard hacker:map
         (check-equal? (hacker:map-fr sub1 '(1 2 3 4))
                       (hacker:map sub1 '(1 2 3 4))))))

   (test-case "append-fr"
     (with-guard hacker:append-fr
       (with-guard hacker:append
         (check-equal? (hacker:append-fr '(42 120) '(1 2 3))
                       (hacker:append '(42 120) '(1 2 3)))
         (check-equal? (hacker:append-fr '(a b c) '(cat dog))
                       (hacker:append '(a b c) '(cat dog))))))

   (test-case "reverse-fr"
     (with-guard hacker:reverse-fr
       (with-guard hacker:reverse
         (check-equal? (hacker:reverse-fr '(a 3 x))
                       (hacker:reverse '(a 3 x))))))

   (test-case "binary->natural-fr"
     (with-guard hacker:binary->natural-fr
       (with-guard hacker:binary->natural
         (check-eqv? (hacker:binary->natural-fr '())
                     (hacker:binary->natural '()))
         (check-eqv? (hacker:binary->natural-fr '(0 0 1))
                     (hacker:binary->natural '(0 0 1)))
         (check-eqv? (hacker:binary->natural-fr '(0 0 1 1))
                     (hacker:binary->natural '(0 0 1 1)))
         (check-eqv? (hacker:binary->natural-fr '(1 1 1 1))
                     (hacker:binary->natural '(1 1 1 1)))
         (check-eqv? (hacker:binary->natural-fr '(1 0 1 0 1))
                     (hacker:binary->natural '(1 0 1 0 1)))
         (check-eqv? (hacker:binary->natural-fr '(1 1 1 1 1 1 1 1 1 1 1 1 1))
                     (hacker:binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1))))))

   (test-case "append-map-fr"
     (with-guard hacker:append-map-fr
       (with-guard hacker:append-map
         (check-equal? (hacker:append-map-fr hacker:countdown (hacker:countdown 5))
                       (hacker:append-map hacker:countdown (hacker:countdown 5))))))

   (test-case "set-difference-fr"
     (with-guard hacker:set-difference-fr
       (with-guard hacker:set-difference
         (check-equal? (list->set (hacker:set-difference-fr '(1 2 3 4 5) '(2 6 4 8)))
                       (list->set (hacker:set-difference '(1 2 3 4 5) '(2 6 4 8)))))))

   (test-case "powerset-fr"
     (with-guard hacker:powerset-fr
       (with-guard hacker:powerset
         (check-equal? (list->set (map list->set (hacker:powerset-fr '(3 2 1))))
                       (list->set (map list->set (hacker:powerset '(3 2 1)))))
         (check-equal? (list->set (hacker:powerset-fr '()))
                       (list->set (hacker:powerset '()))))))
   
   (test-case "snowball"
     (with-guard hacker:snowball
       (check-eqv? (hacker:snowball 12) 1)
       (check-eqv? (hacker:snowball 120) 1)
       (check-eqv? (hacker:snowball 9999) 1)))
   
   (test-case "quine"
     (with-guard hacker:quine
       (let ((ns (make-base-namespace)))
         (check-equal? (eval hacker:quine ns)
                       hacker:quine)
         (check-equal? (eval (eval hacker:quine ns) ns)
                       hacker:quine))))))


(run-tests recursividad 'verbose)
