---
title: Project 1
parent: Projects
---

# Programming Languages: Project 1

<script>
MathJax = {
  tex: {
    inlineMath: [['$', '$'], ['\\(', '\\)']]
  }
};
</script>
<script id="MathJax-script" async
  src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js">
</script>

## Calendar Calculations

You will write 11 Racket functions related to calendar dates. In the descriptions below, a "date" is an Racket list containing three values: 
the first part is the year, the second part is the month, and the third part is the day. For example, September 16, 2023 would be represented 
by the '(2017 9 16). A "reasonable" date would have a positive year, a month between 1 and 12, and a day no greater than 31 (or less 
depending on the month). However, most problems do _not_ assume "reasonable" dates; solutions should work for any date except where noted. A 
"day of year" is a number from 1 to 365 where, for example, 33 represents February 2. (We ignore leap years entirely.)

In the examples, we use  `==>`  to mean "evaluates to."


1.  Write a function  `before?`  that takes two dates and evaluates to  `#t`  or  `#f`. It evaluates to  `#t`  if the first argument is a 
date that comes chronologically before the second argument. (If the two dates are the same, the result is  `#f`.)
    
    Example:  `(before? '(2017 4 2) '(2017 5 1)) ==> #t`
    
2.  Write a function  `number-in-month`  that takes a list of dates and a month (i.e., an integer) and returns how many dates in the list are 
in the month.
    
    Example:  `(number-in-month '((2017 1 2) (2016 2 1) (2019 2 3) (2017 12 1)) 2) ==> 2`
    
3.  Write a function  `number-in-months`  that takes a list of dates and a list of months (i.e., a list of integers) and returns the number 
of dates in the list of dates that are in any of the months in the list of months.  _Assume the list of months has no number repeated (or if 
a number is repeated then dates in that month are counted multiple times)._  Use your answer to the previous problem.
    
    Example: `(number-in-months '((2017 1 2) (2016 2 1) (2019 2 3) (2017 12 1)) '(12 2)) ==> 3`
    
4.  Write a function  `dates-in-month`  that takes a list of dates and a month (i.e., an integer) and returns a list holding the dates from 
the argument list of dates that are in the month.
    
    Example:  `(dates-in-month '((2017 1 2) (2016 2 1) (2019 2 3) (2017 12 1)) 2) ==> '((2016 2 1) (2019 2 3))`
    
5.  Write a function  `dates-in-months`  that takes a list of dates and a list of months (i.e., a list of integers) and returns a list 
holding the dates from the argument list of dates that are in any of the months in the list of months.  _Assume the list of months has no 
number repeated (or if a number is repeated then dates in that month should appear in the result list multiple times)._  Use your answer to 
the previous problem and  `append`.
    
    Example: `(dates-in-months '((2017 1 2) (2016 2 1) (2019 2 3) (2017 12 1)) '(12 2)) ==> '((2017 12 1) (2016 2 1) (2019 2 3))`
    
6.  Write a function  `get-nth`  that takes a list and an integer  `n`  and returns the  `n`th element of the list where the  car  of the 
list is  1st. You may assume the list argument will never have fewer arguments than needed based on the integer argument.  
    
    Example:  `(get-nth '(7 5 3 8 1) 2) ==> 5`
    
    Note that we wrote a very similar function with the same name in class, but this version starts the indices at 1, not 0. This makes the 
following function,  `date->string`, simpler because we usually consider January to be month 1, not month 0.
    
7.  Racket contains a  string  data type that is has similar functionality to strings in Python or Java. For instance, a literal string in 
Racket is a sequence of characters enclosed by double quotes. Write a function  `date->string`  that takes a date and returns a  string  of 
the form  "April 11, 2017"  (for example). Use the function  `string-append`  for concatenating strings and the function  `number->string`  
for converting an integer to a  string. For producing the month part, do  _not_  use a sequence of conditionals. Instead, use a list holding 
12 strings and your answer to the previous problem.
    
    Example:  `(date->string '(2016 7 29)) ==> "July 29, 2016"`
    
    Note: To write a literal list that contains string literals, you should use double quotes around each individual string:
    
    Right: `'("Hello" "World")`  
    Wrong: `'(Hello World)`
    
8.  Write a function  `number-before-reaching-sum`  that takes an integer argument called  `sum`  (which you may assume is 
non-negative) 
and 
a list of integers, and returns an integer. It returns  `n`  if  `sum`  is greater than or equal to the sum of the first  `n`  elements of 
the list, but not greater than or equal to the sum of the first  `n+1`  elements. You may assume that  `sum`  will never be greater than the 
sum of all the numbers in the list.
    
    In other words, this function is answering the question "How many items from the beginning of the list do I need to sum up before I reach 
(or surpass) the argument  `n`?"
    
    Examples:
    
    `(number-before-reaching-sum 2 '(3 2 4 2 5)) ==> 1`  
    `(number-before-reaching-sum 3 '(3 2 4 2 5)) ==> 1`  
    `(number-before-reaching-sum 4 '(3 2 4 2 5)) ==> 2`  
    `(number-before-reaching-sum 5 '(3 2 4 2 5)) ==> 2`  
    `(number-before-reaching-sum 6 '(3 2 4 2 5)) ==> 3`  
    
9.  Write a function  `what-month`  that takes a day of year (i.e., an integer between 1 and 365) and returns an integer representing the 
month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your answer to the previous problem.
    
    Examples:  
    
    `(what-month 1) ==> 1   ; January 1`  
    `(what-month 31) ==> 1  ; January 31`  
    `(what-month 32) ==> 2  ; February 1`  
    `(what-month 138) ==> 5 ; May 18`  
    
1.  Write a function  `month-range`  that takes two days of the year  `day1`  and  `day2`  (integers) and returns a list of integers  `(m1  
m2 ... mn)` where  `m1`  is the month of  `day1`,  `m2`  is the month of  day1 + 1, ..., and  `mn` is the month of  day2. Note the result will 
have length  day2 - day1 + 1  or length 0 if  day1 > day2.
    
    Example:  `(month-range 30 34) ==> '(1 1 2 2 2)`
    
1.  Write a function  `earliest`  that takes a list of dates and returns the earliest date in the list. The argument list is guaranteed to 
contain at least one date. 

    Example: `(earliest '((2017 1 2) (2016 2 1) (2019 2 3) (2017 12 1))) ==> '(2016 2 1)`

## Grading

Solutions should be:

-   Correct
-   Written in good style, including indentation and line breaks
-   Written using features discussed in class. In particular, you must not use any mutation operations nor arrays (even though Racket has 
them).

## Turn-in instructions

-   Put all your solutions in one file.
-   Upload this file to Canvas by the project deadline.

## Debugging in Racket

Debugging in a functional language can be tricky because normally programming in a "pure" functional manner means we don't use any statements 
with side effects, such as printing to the screen. Racket, however, includes plenty of [functions that will display things on the 
screen](https://docs.racket-lang.org/guide/read-write.html), if we temporarily relax the strict rules of functional programming.

I recommend making plenty of use of the  `displayln`  function when debugging. This function takes one argument and prints it to the screen, 
along with a newline at the end. To print multiple things on one line, you have to combine the pieces in some way; the easiest way is with 
the  `list`  function, which makes a list out of its arguments.

`displayln`  must always take one single argument, so if you want to print a blank line by itself, use  `(newline)`

As an example, here's the solution code for our sublist function:

```racket
(define (sublist lst start end)
  (cond ((and (= start 0) (= end 0)) '())
        ((= start 0) (cons (car lst) (sublist (cdr lst) 0 (- end 1))))
        (#t (sublist (cdr lst) (- start 1) (- end 1)))))
```

Here it is again, after adding some debugging code:

```racket
(define (sublist lst start end)
  (newline)
  (displayln (list "lst=" lst "start=" start "end=" end))
  (cond ((and (= start 0) (= end 0)) '())
        ((= start 0)
         (displayln (list "start is 0" "car=" (car lst)))
         (cons (car lst) (sublist (cdr lst) 0 (- end 1))))
        (#t
         (displayln "start is not 0")
         (sublist (cdr lst) (- start 1) (- end 1)))))
```

I've added debugging statements to print at the beginning of each function call, and inside the function call to illustrate which recursive 
case is being triggered.

When I call  `(sublist '(4 5 2 3 7 5) 1 3)`, I now see the following output:

```
> (sublist '(4 5 2 3 7 5) 1 3)
(lst= (4 5 2 3 7 5) start= 1 end= 3)
start is not 0

(lst= (5 2 3 7 5) start= 0 end= 2)
(start is 0 car= 5)

(lst= (2 3 7 5) start= 0 end= 1)
(start is 0 car= 2)

(lst= (3 7 5) start= 0 end= 0)
'(5 2)
> 
```

Notice how the `displayln` output is easily distinguished from the return value of the function call by the colors of the text.

**Like you do in Python or Java, always comment out or remove debugging statements before turning in your code!**

## Other notes on Racket

-   Because the DrRacket interpreter supports multiple varieties of the Racket language, always begin your source code files with a line 
containing the directive
    
    `#lang racket`
    
    and nothing else.
    
-   Racket contains two types of comments. A semicolon begins a  _line comment_. Everything past the semicolon to the end of the current line 
is ignored by Racket.
    
    `(define (add1 n) (+ 1 n))  ; This function adds one to its argument.`
    
    _Block comments_  in Racket are enclosed by  `#|`  and  `|#`, similar to how Java uses  `/*`  and  `*/`. These comments may extend over 
multiple lines.
```    
    #| Here is a single-line comment. |#
    
    #| This comment
    has multiple lines!  |#
```    
-   To write a literal list that contains sub-lists, do not use additional quotes:
    
    Right: `'(1 2 (3 4))`
    Wrong: `'(1 2 '(3 4))`
