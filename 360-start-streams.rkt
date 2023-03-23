#lang racket
(define-syntax-rule (stream-cons first rest)
  (cons first (delay rest)))

(define the-empty-stream '())

(define (stream-null? stream)
  (null? stream))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-enumerate s n)
  (if (= n 0) '()
      (cons (stream-car s) (stream-enumerate (stream-cdr s) (- n 1)))))

(define (stream-map f stream)
  (stream-cons (f (stream-car stream)) (stream-map f (stream-cdr stream))))


;; Samples:

(define (make-constant-stream v)
  (stream-cons v (make-constant-stream v))

(define ones (stream-cons 1 ones))

(define (ints-from n)
  (stream-cons n (ints-from (+ n 1)))

(define ints-from-1 (ints-from 1))
