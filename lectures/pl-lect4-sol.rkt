#lang racket

(define (length lst)
  (if (null? lst) 0
      (+ 1 (length (cdr lst)))))

(define (length-nested lst)
  (cond ((null? lst) 0)
        ((list? (car lst))
         (+ (length-nested (car lst))
            (length-nested (cdr lst))))
        (#t (+ 1 (length-nested (cdr lst))))))

(define (get-nth lst n)
  (if (= n 0) (car lst)
      (get-nth (cdr lst) (- n 1))))

(define (sublist lst start end)
  (cond ((and (= start 0) (= end 0)) '())
        ((= start 0) (cons (car lst) (sublist (cdr lst) 0 (- end 1))))
        (#t (sublist (cdr lst) (- start 1) (- end 1)))))

(define (sum-nested lst)
  (cond ((null? lst) 0)
        ((list? (car lst))
         (+ (sum-nested (car lst))
            (sum-nested (cdr lst))))
        (#t (+ (car lst) (sum-nested (cdr lst))))))

(define (flatten lst)
  (cond ((null? lst) '())
        ((list? (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
        (#t (cons (car lst) (flatten (cdr lst))))))

(define (scale lst factor)
  (if (null? lst) '()
      (cons (* factor (car lst)) (scale (cdr lst) factor))))

(define (insert elt lst)
  (cond ((null? lst) (list elt))
        ((< elt (car lst)) (cons elt lst))
        (#t (cons (car lst) (insert elt (cdr lst))))))

(define (insertion-sort lst)
  (define (helper unsorted sorted)
    (if (null? unsorted) sorted (helper (cdr unsorted) (insert (car unsorted) sorted))))
  (helper lst '()))

(define (merge lst1 lst2)
  (cond ((null? lst1) lst2)
        ((null? lst2) lst1)
        ((< (car lst1) (car lst2)) (cons (car lst1) (merge (cdr lst1) lst2)))
        (#t (cons (car lst2) (merge lst1 (cdr lst2))))))




