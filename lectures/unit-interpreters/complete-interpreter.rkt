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
    ((lambda? expr) (eval-lambda expr env))
    ((call? expr) (eval-call expr env))
    
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

; lambda? returns true if expr is a lambda expression, which looks like (lambda var expr).
(define (lambda? expr) (equal? 'lambda (car expr)))

; call? returns true if expr is an call expression, which looks like (call expr1 expr2).
(define (call? expr) (equal? 'call (car expr)))

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

; For eval-definition, expr is always of the form: (define varname expr-for-var)
;   varname must be a variable name (a symbol), and expr1 can expr-for-var to anything.
(define (eval-definition expr env)
  (let ((varname (cadr expr))
        (expr-for-var (caddr expr)))
    (hash-set! (car env) varname (mini-eval expr-for-var env))
    'done))   ; the done is just here as the return value of the let

; For eval-ifzero, expr is always of the form: (ifzero test-expr true-expr false-expr)
;   test-expr must evaluate to a number, and true-expr and false-expr can evaluate to anything.
(define (eval-ifzero expr env)
  (let ((test-expr (cadr expr))
        (true-expr (caddr expr))
        (false-expr (cadddr expr)))
    (if (= 0 (mini-eval test-expr env)) 
        (mini-eval true-expr env) 
        (mini-eval false-expr env))))

; For eval-lambda, expr is always of the form: (lambda argname body-expr)
;   argname must be a variable name (a symbol), and body is any expression.
(define (eval-lambda expr env)
  (let ((argname (cadr expr))
        (body-expr (caddr expr)))
    (list 'closure argname body-expr env)))

; For eval-call, expr is always of the form: (call closure-expr arg-expr)
;   closure-expr must eval to a closure, arg-expr can evaluate to anything.
(define (eval-call expr env)
  (let ((closure-expr (cadr expr))
        (arg-expr (caddr expr)))
    (mini-apply (mini-eval closure-expr env) (mini-eval arg-expr env))))

; Mini-apply is used to evaluate the result of calling a user-defined function.  
;  Args: closure must be a closure type (a list of four values), and argval must be
;  a the value of the argument the closure will be evaluated on.
;  Remember: a four values in a closure type are:
;    the symbol 'closure (car)
;    the argument name (cadr)
;    the body of the lambda (caddr)
;    the environment that was current when the closure was created (cadddr)
(define (mini-apply closure argval)
  (let ((new-frame (make-hash))
        (argname (cadr closure))
        (body-expr (caddr closure))
        (closure-env (cadddr closure)))
    (hash-set! new-frame argname argval)
    (let ((new-env (cons new-frame closure-env)))
      (mini-eval body-expr new-env))))

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
              (display "global-environment: ")
              (display global-env)
              (display "\nmini-eval output: ")
              (displayln output)
              (read-eval-print-loop))
            'done)))
    (read-eval-print-loop)))

; (define square (lambda p (mul p p)))
; (define fact (lambda n (ifzero n 1 (mul n (call fact (sub n 1))))))
; (define fib (lambda n (ifzero n 0 (ifzero (sub n 1) 1 (add (call fib (sub n 1)) (call fib (sub n 2)))))))
; (define make-adder (lambda s (lambda t (add s t))))
; (define add1 (call make-adder 1))
; (call add1 4)