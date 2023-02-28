#lang racket
; Example 1
(define x 1)
(define (f y) (+ x y))
(define q (f 5))
(define y 4)
(define z (let ((x 2)) (f (+ x y))))

; Example 2 (returning a function)
(define x 1)
(define (f y) (lambda (z) (+ x y z)))
(define g (f 4))
(define z (g 6))

; Section 3 (passing a function and let)
(define (f g) (let ((x 3)) (g 2)))
(define x 4)
(define (h y) (+ x y))
(define z (f h))

; foldr examples
(define (f1 lst) (foldr + 0 lst))
(define (f2 lst) (foldr (lambda (x y) (and (>= x 0) y)) #t lst))

(define (f3 lo hi lst) 
  (foldr (lambda (x y) 
            (+ (if (and (>= x lo) (<= x hi)) 1 0) y)) 0 lst))

(define (f4 g lst) 
  (foldr (lambda (x y) (and (g x) y)) #t lst)) 