Exercises for Lexical Scoping, day 2

; === Program 1 ===

(define (f x)
  (lambda (y) (+ x y)))
  
(define g (f 3))
(define z (g 4))

; === Program 2 ===

(define (f g)
  (let ((x 2))
    (g x)))
    
(define (h y)
  (let ((x 3))
    (lambda (y) (+ x y))))
    
(define m (f h))
(define n (m 7))

; === Program 3 ===

(define (f x)
  (let ((y 3)
        (g (lambda (z) (+ x z))))
    (g (- y 1))))

(f 4)

; Questions for program 3:
; Suppose the definition of g were changed to (+ x y z).  Would the program still work?
   Why or why not?
; Keeping your answer to the previous question in mind, suppose we made g a recursive
;  function.  Would the program still work?