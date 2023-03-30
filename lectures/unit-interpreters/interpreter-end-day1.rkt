#lang racket

; mini-eval is the heart of the interpreter.  It takes an expression (can be anything),
; and an environment (must be a list of hashtables), and returns what the expression
; evaluates to while using the environment to look up the values of variables.

(define (mini-eval expr env)
  (display "  Evaluating: ")
  (displayln expr)
  (cond 
    ((number? expr) expr)
    ((symbol? expr) (lookup-variable-value expr env))
    
    ((add? expr) (eval-add expr env))
    ((subtract? expr) (eval-subtract expr env))
    ((multiply? expr) (eval-multiply expr env))
    ((divide? expr) (eval-divide expr env))
    ((exponent? expr) (eval-exponent expr env))
    
    ((definition? expr) (eval-definition expr env))
    ((ifzero? expr) (eval-ifzero expr env))
    
    (#t (error "Invalid expression:" expr))))

; add? returns true if expr is an addition expression, which looks like (add expr1 expr2).
(define (add? expr) (equal? 'add (car expr)))

; subtract? returns true if expr is a subtraction expression, which looks like (sub expr1 expr2).
(define (subtract? expr) (equal? 'sub (car expr)))

; multiply? returns true if expr is a multiplication expression, which looks like (mul expr1 expr2).
(define (multiply? expr) (equal? 'mul (car expr)))

; divide? returns true if expr is a division expression, which looks like (div expr1 expr2).
(define (divide? expr) (equal? 'div (car expr)))

; exponent? returns true if expr is an exponent expression, which looks like (exp expr1 expr2).
(define (exponent? expr) (equal? 'exp (car expr)))

; definition? returns true if expr is a define expression, which looks like (define symbol expr).
(define (definition? expr) (equal? 'define (car expr)))

; ifzero? returns true if expr is an ifzero expression, which looks like (ifzero expr1 expr2 expr3).
(define (ifzero? expr) (equal? 'ifzero (car expr)))

; eval-add is called by mini-eval to handle evaluating an addition expression: (add expr1 expr2).
(define (eval-add expr env) (+ (mini-eval (cadr expr) env) (mini-eval (caddr expr) env)))

; eval-subtract is called by mini-eval to handle evaluating a subtraction expression: (sub expr1 expr2).
(define (eval-subtract expr env) (- (mini-eval (cadr expr) env) (mini-eval (caddr expr) env)))

; eval-multiply is called by mini-eval to handle evaluating an addition expression: (mul expr1 expr2).
(define (eval-multiply expr env) (* (mini-eval (cadr expr) env) (mini-eval (caddr expr) env)))

; eval-divide is called by mini-eval to handle evaluating an addition expression: (div expr1 expr2).
(define (eval-divide expr env) (/ (mini-eval (cadr expr) env) (mini-eval (caddr expr) env)))

; eval-exponent is called by mini-eval to handle evaluating an addition expression: (exp expr1 expr2).
(define (eval-exponent expr env) (expt (mini-eval (cadr expr) env) (mini-eval (caddr expr) env)))

; eval-definition assumes expr is of the form (define var expr).
; We evaluate the sub-expression [i.e., (caddr expr)] using mini-eval,
; then bind the variable [i.e., (cadr expr)] to its value in the hash table.
(define (eval-definition expr env) 
  (hash-set! (car env) (cadr expr) (mini-eval (caddr expr) env)))

; eval-ifzero is called for expressions that look like (ifzero expr1 expr2 expr3).
; If expr1 evaluates to zero, then we evaluate and return expr2, otherwise expr3.
(define (eval-ifzero expr env) 
  (if (= 0 (mini-eval (cadr expr) env)) 
      (mini-eval (caddr expr) env) 
      (mini-eval (cadddr expr) env)))

; lookup-variable-value takes a symbol representing a variable and an environment (a list
; of hash tables).  The function checks to see if the variable is stored inside the first
; hash table [i.e., (car env)].  If it's not, it recurses on the next hash table.  If
; there is no next hash table, it returns an error.
(define (lookup-variable-value var env)
  (cond
    ((null? env) (error "unbound variable:" var))
    ((hash-has-key? (car env) var) (hash-ref (car env) var))
    (#t (lookup-variable-value var (cdr env)))))

; run starts the interpreter.  It starts a loop (a REPL) that reads an expression
; from the keyboard, calls mini-eval on it, and prints the result.
(define (run)
  (let ((global-env (list (make-hash))))
    (define (read-eval-print-loop)
      (display "mini-eval input: ")
      (let ((input (read)))
        (if (not (equal? input 'end))
            (let ((output (mini-eval input global-env)))
              (display "mini-eval output: ")
              (displayln output)
              (read-eval-print-loop))
            'done)))
    (read-eval-print-loop)))