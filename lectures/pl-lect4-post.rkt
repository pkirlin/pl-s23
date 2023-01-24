#lang racket

(define (get-nth lst n)
  (if (= n 0) (car lst)
      (get-nth (cdr lst) (- n 1))))

(define (sublist lst start end)
  (cond ((and (= start 0) (= end 0)) '())
        ((= start 0) (cons (car lst) (sublist (cdr lst) 0 (- end 1))))
        (#t (sublist (cdr lst) (- start 1) (- end 1)))))

(define (length-nested lst)
  (cond ((null? lst) 0)
        ((list? (car lst))
         (+ (length-nested (car lst))
            (length-nested (cdr lst))))
        (#t (+ 1 (length-nested (cdr lst))))))

(define (sum-list-nested lst)
  (cond ((null? lst) 0)
        ((list? (car lst))
         (+ (sum-nested (car lst))
            (sum-nested (cdr lst))))
        (#t (+ (car lst) (sum-nested (cdr lst))))))

(define (scale lst factor)
  (if (null? lst) '()
      (cons (* factor (car lst)) (scale (cdr lst) factor))))

(define (flatten lst)
  (cond ((null? lst) '())
        ((list? (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
        (#t (cons (car lst) (flatten (cdr lst))))))

