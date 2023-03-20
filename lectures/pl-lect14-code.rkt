;; Env diagrams

(define (new-stack)
  (let ((the-stack '()))
    (define (dispatch method-name)
      (cond ((eq? method-name 'empty?) empty?)
            ((eq? method-name 'push) push)
            ((eq? method-name 'pop) pop)
            (#t (error "Bad method name"))))
    (define (empty?) (null? the-stack))
    (define (push item) (set! the-stack (cons item the-stack)))
    (define (pop) 
      (if (null? the-stack) (error "Can't pop an empty stack")
          (let ((top-item (car the-stack)))
            (set! the-stack (cdr the-stack))
            top-item)))
    dispatch))   
	
(define S (new-stack))
((S 'push) 5)

(define b 3) 
(define f (lambda (x) (* 2 (+ x b)))) 
(define c (+ b 4)) 
(set! b 5)
(define z (f 4))   
(define w c)       



; does not mutate contents:
(define x (cons 14 '()))
(define y x)
(set! x (cons 42 '()))
(define fourteen (car y))

