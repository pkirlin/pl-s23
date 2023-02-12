; Programming Languages
; Tail recursion and using accumulators 

#lang racket

; not tail recursive:
(define (fact1 n)
  (if (= n 0) 1
      (* n (fact1 (- n 1)))))

#|
  (fact 3)
     => (* 3 (fact 2))
     => (* 3 (* 2 (fact 1)))
     => (* 3 (* 2 (* 1 (fact 0))))
     => (* 3 (* 2 (* 1 1)))
     => (* 3 (* 2 1))
     => (* 3 2)
     => 6
|#

; tail recursive version
(define (fact2 n)
  (define (fact2-helper n acc)
    (if (= n 0) acc
        (fact2-helper (- n 1) (* acc n))))
  (fact2-helper n 1))

#|
   (fact2 3)
     => (fact2-helper 3 1)
     => (fact2-helper 2 3)
     => (fact2-helper 1 6)
     => (fact2-helper 0 6)
     => 6
|#

; not tail-recursive
(define (length lst)
  (if (null? lst) 0
      (+ 1 (length (cdr lst)))))

; tail-recursive
(define (length-tr lst)
  (define (length-tr-helper lst acc)
    (if (null? lst) acc
        (length-tr-helper (cdr lst) (+ 1 acc))))
  (length-tr-helper lst 0))

; not tail-recursive AND takes quadratic time!
(define (rev lst)
  (if (null? lst) '()
      (append (rev (cdr lst)) (list (car lst)))))

; tail-recursive and takes linear time (much better)
(define (rev-tr lst)
  (define (rev-tr-helper lst acc)
    (if (null? lst) acc
        (rev-tr-helper (cdr lst) (cons (car lst) acc))))
  (rev-tr-helper lst '()))

#| tail recursion usually possible for functions that process lists, but not so much
   for functions that process tree structures (without fancier rewriting involving 
   higher-order functions that gives back much of the performance advantage) 
|#

; Compare with Python code using while loops for factorial, length, and reverse:
#|
def fact2(n):
  acc = 1
  while n != 0:
  	acc = acc * n
  	n = n - 1
  return acc
  
def length(lst):
	acc = 0
	while lst != []:
		acc = acc + 1
		lst = lst[1:]
	return acc
	
def reverse(lst):
  acc = []
  while lst != []:
    acc = [lst[0]] + acc
    lst = lst[1:]
  return acc		
|#

(define (sum-tr lst)
  (define (sum-tr-helper lst total)
    (if (null? lst) total
        (sum-tr-helper (cdr lst) (+ total (car lst)))))
  (sum-tr-helper lst 0))

(define (max-tr lst)
  (define (max-tr-helper lst maxSoFar)
    (if (null? lst) maxSoFar
        (if (> (car lst) maxSoFar)
            (max-tr-helper (cdr lst) (car lst))
            (max-tr-helper (cdr lst) maxSoFar))))
  (max-tr-helper (cdr lst) (car lst)))