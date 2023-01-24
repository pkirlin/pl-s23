#lang racket
; COMP 360
; Lecture 4

; Starting example: sum up all numbers in a list
(define (sum-list lst)
  (if (null? lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

; Starting example: generate a list of numbers, counting down
(define (countdown num)
  (if (= num 0)
      '()
      (cons num (countdown (- num 1)))))

; compare the regular length function to the 
; one that works with nested lists:

(define (length lst)
  (if (null? lst) 0
      (+ 1 (length (cdr lst)))))

(define (length-nested lst)
  (cond ((null? lst) 0)
        ((list? (car lst))
         (+ (length-nested (car lst))
            (length-nested (cdr lst))))
        (#t (+ 1 (length-nested (cdr lst))))))

#| Try to write these functions:

get-nth: Treats an list like an array and returns the element in the list at the
	index given as an argument.  Assume indices start at zero.
	
	Ex: (get-nth '(2 4 6 8 10) 3) ==> 8
	
sublist: Given integers start and end, treats a list like an array and returns the 
  sublist containing elements start, start+1, ..., end-1.  (So this works like array 
  slices in Python, in that the slice starts at "start" and ends at one before "end.")
  
  Hint: This can be done with calls to get-nth, but it's probably more efficient to write
  a single recursive function rather than delegating to get-nth.
  
  Ex: (sublist '(2 4 6 8 10) 1 3) ==> '(4 6)

sum-list-nested: Works like sum-list, except sums up all elements, including those
	from nested lists.
	
	Ex: (sum-nested '((1 2) () 3 ((4) 5))) ==> 15
	
scale: Takes a list of numbers and returns a new list with all the numbers scaled by
	a given factor.
	
	Ex: (scale '(1 2 3) 2) ==> '(2 4 6)
	
	Note that if you write this correctly, your function should end up being polymorphic
	in that it can accept integers, reals, or rationals in the list or the factor.
	
keep-odds: Takes a list of numbers and returns a new list with all only the numbers
	that are odd retained.
	
	Hint: There's a built-in function (odd? p) that returns #t if p is odd.
	
	Ex: (keep-odds '(1 2 3)) ==> '(1 3)
	
flatten: Returns a list with all elements from all nested lists at the top level.

  Ex: (flatten '((1 2) () 3 ((4) 5))) ==> '(1 2 3 4 5)
	
insert-into-sorted: Takes a list of numbers that must be sorted, and another number,
	and returns a new list with the new number inserted into the appropriate place
	in the list such that the new list is also sorted.

        Ex: (insert-into-sorted 5 '(1 2 4 6 10)) ==> '(1 2 4 5 6 10)
	
insertion-sort: Write a function that takes an unsorted list of numbers and uses the
	previous function to sort them.
	
	Hint: You'll probably want to use a helper function (a locally-defined function that
	insert-sort will call).
	
merge-sorted-lists: Takes two lists of sorted numbers and merges them into a single
	sorted list.

|#

