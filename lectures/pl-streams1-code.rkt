#lang racket

;; Delayed evaluation

;; These don't work:
(define (my-if-bad x y z) 
  (if x y z))

(define (fact-wrong n) 
    (my-if-bad (= n 0)
               1
               (* n (fact-wrong (- n 1)))))

;; This DOES work:
(define (my-if x y z) 
  (if x (y) (z)))

(define (fact n) 
    (my-if (= n 0)
           (lambda () 1)
           (lambda () (* n (fact (- n 1))))))

; simulate a long computation time
(define (compute-answer-to-life) 
  (begin (sleep 3) 42))

; create a thunk for the answer
(define answer 
   (lambda () (compute-answer-to-life)))

;(answer) ; 3 second pause, then 42
;(answer) ; 3 second pause again, then 42

;; Now we introduce promises: a promise is a data type that stores
; a computation (as a thunk) but remembers the thunk's return value
; in case it's needed again.

; make-promise: create a promise data type for the thunk argument.
; eval-promise: evaluate the promise (either run the thunk and return result, or just
;    return the already-computed result).

(define (make-promise thunk)
  (mcons #f thunk))

(define (eval-promise p)
	(if (mcar p)
      (mcdr p)
	    (begin (set-mcar! p #t)
              (set-mcdr! p ((mcdr p)))
              (mcdr p))))
			  
(define answer2 (make-promise (lambda () (compute-answer-to-life))))
;(eval-promise answer2) ; 3 second pause, then 42
;(eval-promise answer2) ; instant 42

(require racket/promise)
(define answer3 (delay (compute-answer-to-life)))
;(force answer3) ; 3 second pause, then 42
;(force answer3) ; instant 42

;; Streams

; don't use this version of stream-cons; it won't work b/c rest is not 
; automatically thunked.  Use the special form version from streams.rkt.
;(define (stream-cons first rest)
;  (cons first (delay rest))
  
; this is the correct syntax for stream-cons, but don't worry about the 
; "define-syntax-rule" part.  It's the magic that lets us define a new special form.
(define-syntax-rule (stream-cons first rest)
  (cons first (delay rest)))

(define the-empty-stream '())

(define (stream-null? stream)
  (null? stream))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))
  

(define (while condition body)
  (if (condition)
      (begin (body)
             (while condition body))
      "done"))

(define (my-length lst)
  (let ((count 0))
    (while (lambda () (not (null? lst)))
           (lambda () (begin (set! count (+ count 1)) (set! lst (cdr lst)))))
    count))
                  
