(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))
(define ints-from-2 (integers-from 2))

(define ints-from-2-alt 
  (stream-cons 2 
    (stream-map (lambda (x) (+ x 1)) ints-from-2-alt)))

(define ints-from-2-alt-alt
  (stream-cons 2 
      (stream-map2 + infinite-ones ints-from-2-alt-alt)))

(define (make-fib-stream a b)
  (stream-cons a (make-fib-stream b (+ a b))))

(define fibs1 (make-fib-stream 0 1))

(define fibs 
 (stream-cons 0
    (stream-cons 1
       (stream-map2 + (stream-cdr fibs) fibs))))

(define (not-divisible-by s div)
  (stream-filter (lambda (x) (> (remainder x div) 0)) s))

(define (sieve s)
  (stream-cons
   (stream-car s)
   (sieve (not-divisible-by s (stream-car s)))))

(define primes (sieve ints-from-2))

;; Memoization:

(define (slow-fib n)
  (if (or (= n 0) (= n 1)) n
    (+ (slow-fib (- n 1))
       (slow-fib (- n 2)))))

(define fast-fib
 (let ((cache '()))
  (define (lookup-in-cache cache n)
   (cond ((null? cache) #f)
         ((= (caar cache) n) (cadar cache))
         (#t (lookup-in-cache (cdr cache) n))))
    
 (lambda (n)
  (if (or (= n 0) (= n 1)) n
      (let ((check-cache (lookup-in-cache cache n)))
       (cond ((not check-cache) 
               (let ((answer (+ (fast-fib (- n 1))
                                (fast-fib (- n 2)))))
                (set! cache (cons (list n answer) cache))
                     answer))
             (#t check-cache)))))))
