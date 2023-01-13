#lang racket
; CS 360 Lecture 1
; Our first program!

(define x 3)
; adds x -> 3 to the current environment

(define y 7)
; adds y -> 7 to the current environment

; to evaluate an addition, evaluate the subexpressions and add 
; to evaluate a variable, lookup its value in the environment  
(define z (- x y))
; evaluates (- x y), which results in -4, then
; adds z -> -4 to the current environment

(define q (* (+ x 2) (- z 3)))
; evaluates the whole math expression, which results in -35
; adds q -> -35 to the current environment

(define abs-of-z (if (< z 0) (- z) z))

