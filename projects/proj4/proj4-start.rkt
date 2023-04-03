#lang racket
(require rsound)

;; Include these functions at the top of every program that uses streams.
(define-syntax-rule (stream-cons first rest)
  (cons first (delay rest)))
(define the-empty-stream '())
(define stream-null? null?)
(define stream-car car)
(define (stream-cdr stream)
  (force (cdr stream)))

;; These next two functions use the Racket sound library.
;; You don't have to know how they work.
(define (stream-to-sound stream)
  (define (next-value)
    (let ((v (stream-car stream)))
      (set! stream (stream-cdr stream))
      v))
  next-value)

; Play a stream as a sound.
(define (play-stream s)
  (signal-play (stream-to-sound s)))

; Turn a finite stream into a list.
(define (stream->list stream)
  (if (stream-null? stream) the-empty-stream
      (cons (stream-car stream) (stream->list stream))))

; Get a specific element of a stream (zero-based indexing).
(define (stream-ref stream index)
  (if (= index 0) (stream-car stream)
      (stream-ref (stream-cdr stream) (- index 1))))

; Get a list of the first n elements of a stream.
(define (stream-enumerate stream n)
  (if (= n 0) '()
      (cons (stream-car stream)
            (stream-enumerate (stream-cdr stream) (- n 1)))))

; Construct a new stream by transforming an existing one.
; f must be a function of one argument.
(define (stream-map f s)
  (if (stream-null? s) the-empty-stream
      (stream-cons (f (stream-car s)) (stream-map f (stream-cdr s)))))

; Construct a new stream by transforming an existing one.
; f must be a function of two arguments.
(define (stream-map2 f s1 s2)
  (if (or (stream-null? s1) (stream-null? s2)) the-empty-stream
      (stream-cons (f (stream-car s1) (stream-car s2)) (stream-map2 f (stream-cdr s1) (stream-cdr 
s2)))))

; Construct a new stream by filtering an existing one.
(define (stream-filter f s)
  (if (stream-null? s) the-empty-stream
      (if (f (stream-car s)) (stream-cons (stream-car s) (stream-filter f (stream-cdr s)))
          (stream-filter f (stream-cdr s)))))


