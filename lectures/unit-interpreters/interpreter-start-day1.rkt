#lang racket

; mini-eval is the heart of the interpreter.  It takes an expression (can be anything),
; and an environment (must be a list of hashtables), and returns what the expression
; evaluates to while using the environment to look up the values of variables.

(define (mini-eval expr env)
  (display "  Evaluating: ")
  (displayln expr)
  (cond 
    ((number? expr) expr)
    ;((symbol? expr) (lookup-variable-value expr env))
    
    ((add? expr) (eval-add expr env))
    ;((subtract? expr) (eval-subtract expr env))
    ;((multiply? expr) (eval-multiply expr env))
    ;((divide? expr) (eval-divide expr env))
    ;((exponent? expr) (eval-exponent expr env))
    
    ;((definition? expr) (eval-definition expr env))
    ;((ifzero? expr) (eval-ifzero expr env))
    
    (#t (error "Invalid expression:" expr))))

; add? returns true if expr is an addition expression, which looks like (add expr1 expr2).
(define (add? expr) (equal? 'add (car expr)))

; eval-add is called by mini-eval to handle evaluating an addition expression.
(define (eval-add expr env) (+ (cadr expr) (caddr expr)))

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