#lang racket
;;; Function closure idioms

;; Composition
(define (compose f g) (lambda (x) (f (g x))))

; 3rd version is best: clearly defines sqrt-of-abs as the composition of two
; functions.
(define (sqrt-of-abs i) (sqrt (abs i)))
;(define (sqrt-of-abs i) ((compose sqrt abs) i))
;(define sqrt-of-abs (compose sqrt abs))

;; Currying
(define (expt-curried x) (lambda (y) (expt x y)))

; call like this:
((expt-curried 4) 2)

;(define (double x) (* 2 x))
;(define double (curry * 2))

; shorten lambda expressions:
;Old way: (map (lambda (x) (+ x 1)) '(1 2 3))
; new way:
(map (curry + 1) '(1 2 3))

(define get-month (curry list-ref '(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec)))

(define get-month-better
  (compose
    (curry list-ref '(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec))
    (curryr - 1)))

(define (eval-polynomial coeff x)
  (if (null? coeff) 0
      (+ (* (car coeff) (expt x (- (length coeff) 1)))
         (eval-polynomial (cdr coeff) x))))

;(define (make-polynomial coeff) 
;  (lambda (x) . . . rest of the function above))

; curried version
(define make-polynomial (curry eval-polynomial))

; now we can do:
(define poly (make-polynomial '(3 2 1)))
(poly 3)

(map (compose (curry + 2) (curry * 4)) '(1 2 3))

(filter (curry < 10) '(6 8 10 12))
(filter (curryr > 10) '(6 8 10 12))

(define (sum-list-ok lst) (foldr + 0 lst))

(define sum-list-super-cool (curry foldr + 0))

(andmap (curryr > 7) '(8 9 10))
(ormap (curryr > 7) '(4 5 6 7 8))
(ormap (curryr > 7) '(4 5 6))

(define contains7 (curry ormap (curry = 7)))
(define all-are7 (curry andmap (curry = 7)))

(define (zip lst1 lst2)
  (if (null? lst1) '()
      (cons (list (car lst1) (car lst2)) 
            (zip (cdr lst1) (cdr lst2)))))

(define countup (curry range 1))

(define (add-numbers lst) 
  (zip (countup (length lst)) lst))

;; Callbacks
; while there are ways to do callbacks without mutation, side effects
; are common with this idiom, so we show that way

; this var will store a list of functions of a single argument
(define callbacks '())

(define (add-callback func) 
  (set! callbacks (cons func callbacks)))
  
; presumably a low-level keyboard driver would call this function.
; key-press then calls all of the functions in callbacks.
(define (key-press which-key)
  ; for-each is like map, but doesn't make a list of the results.
  ; (in other words, it just calls the function on each element in the list)
  (for-each (lambda (func) (func which-key)) callbacks))
  
; Adds a callback that prints the message if the key is hit.
(define (print-if-pressed key message)
  (add-callback (lambda (key2) (if (string=? key key2) (begin (display message) (newline)) #f))))

; Another callback that increments a variable to count the total number of presses.
(define count-presses 0)
(add-callback 
   (lambda (key) 
     (set! count-presses (+ 1 count-presses)) 
     (display "total presses = ")
     (display count-presses)
     (newline)))
     
; A better way to define the callback that counts key presses: this one encapsulates
; the count-presses variable so it will be part of a frame that only the callback
; can access.  count-presses isn't part of the global frame so 
(let ((count-presses 0))
  (add-callback 
   (lambda (key) 
     (set! count-presses (+ 1 count-presses)) 
     (display "total presses = ")
     (display count-presses)
     (newline))))

     
;; Defining an abstract data type with closures.
;; Simulates OOP.

(define (new-stack)
  (let ((the-stack '()))
    (define (dispatch method-name)
      (cond ((eq? method-name 'empty?) empty?)
            ((eq? method-name 'push) push)
            ((eq? method-name 'pop) pop)
            (#t (error "Bad method name"))))
    (define (empty?) (null? the-stack))
    (define (push item) (set! the-stack (cons item the-stack)))
    (define (pop) 
      (if (null? the-stack) (error "Can't pop an empty stack")
          (let ((top-item (car the-stack)))
            (set! the-stack (cdr the-stack))
            top-item)))
    dispatch))


