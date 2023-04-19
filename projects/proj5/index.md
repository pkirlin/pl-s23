---
title: Project 5
parent: Projects
---

Final Project: JRacket
======================

_You may work with a partner for this project. If you choose to do so, only turn in one copy of the project 
with both of your names in a comment at the top of Main.java._

[Get the startup code here.](jracket.zip) Unzip the file, and if you want to use IntelliJ, and drag the entire jracket folder into the 
"src" directory of a new IntelliJ project.

For this project, you will write a Racket interpreter in Java. Your interpreter will support a larger 
subset of Racket than our in-class version (Mini-Racket), but you will not implement every feature of the 
entire language. We will call this language **JRacket**.

This project is structured similarly to our in-class Mini-Racket interpreter, but since we're in Java, 
we're using an OOP style. In other words, instead of having a single `eval()` function, we have a 
`RacketExpression` class with an `eval()` method that other expression classes will override.

The types of expressions you must support are:

*   Integers; implemented in the `RacketInteger` class.
*   Booleans; implemented in the `RacketBoolean` class.
*   Symbols; implemented in the `RacketSymbol` class.
*   Lists; implemented in the `RacketList` class. Note that lists in JRacket serve two purposes (just like 
in regular Racket): they are used as a data structure, e.g., `'(1 2 3)`, but also to represent statements 
in the language, such as `'(define x 3)`.
*   Functions; implemented in the `RacketFunction` class. There are two subtypes: `RacketPrimitiveFunction` 
(a function defined in terms of Java code, similar to add/sub/mul in Mini-Racket, in that we had to 
hard-code them in the interpreter), and `RacketClosure` (a function defined in terms of a lambda 
expression).

Running the interpreter
-----------------------

Run the interpreter by running the `main()` method in the `Main` class. It should run "out of the box," 
though it's not particularly powerful at the start. **You can type end at the prompt to quit the 
interpreter.** You should be able to interact with the interpreter by typing in integers, booleans, or 
quoted expressions (these are already implemented for you):

```
>>> 4
==> 4

>>> #t
==> #t

>>> '(1 2 3)
      Evaluating: (quote (1 2 3))
==> (1 2 3)

>>> 'x
      Evaluating: (quote x)
==> x
```

The basic interpreter is flexible enough that if you make a parenthetical or syntactical mistake, it will 
detect it and not kick you out of the interpreter:

```
>>> '(3 4))
jracket.ParsingException: Too many closing parens in '(3 4)).  Already parsed [', [3, 4]]

>>> #tf
jracket.ParsingException: Cannot parse boolean value: #tf
```

Eval/Apply in JRacket
---------------------

Because we're writing our interpreter in Java, instead of a having one `eval()` and one `apply()`, we have 
a `RacketExpression` class that has an abstract method called `eval()` that subclasses will override. 
Furthermore, the `RacketFunction` class has an `apply()` method that its subclasses will override.

Diving in
---------

What you will need to to is enhance the interpreter to deal with the types of expressions listed below. You 
should refer to the Mini-Racket implementation (and your class notes, the slides, etc) for guidance; we are 
implementing this interpreter in a very similar fashion.

The major difference between this interpreter and Mini-Racket's interpreter is that we have multiple 
`eval()` methods, since each subtype of `RacketExpression` is its own class. The first thing you should do 
is take a look at the `eval()` methods in `RacketInteger` and `RacketBoolean` and notice how they work 
(they're very simple). For instance, `RacketInteger`'s eval() method reflects how in Mini-Racket we tested 
if an expression was a number and if so, we just returned the expression itself (because numbers, when 
evaluated, return themselves).

I suggest you add things in a slightly different order than we did in class:

1.  **Variables:** You will need to familiarize yourself with the `Frame` class, which stores a single 
frame of an environment, and has a pointer to a parent `Frame`. (There is no Environment class; a `Frame` 
suffices to represent an entire environment because it has a pointer to its parent `Frame`). In `Frame`, 
implement the `lookupVariableValue()` and `defineVariable()` methods. The comments in the code should tell 
you what to do. You can ignore `setVariable()`.
    
    Now, go to RacketSymbol and edit the eval() method to call lookupVariableValue() just like Mini-Racket 
does when it sees a symbol.
    
    Next, you now need to edit RacketList's eval() method to support expressions of the type (define x 3). 
Notice how RacketList's eval() dispatches to evalDefine(), evalLambda(), etc, just like Mini-Racket. **Take 
a look at evalQuote() first. It's already written for you, but it will guide you in writing the other 
eval_XYZ_() methods.**
    
    Write evalDefine(). This method should call defineVariable().
    
    You now should be able to do the following:
    
    ```
    >>> (define x 3)
          Evaluating: (define x 3)
    ==> done
    
    >>> x
          Evaluating: x
    ==> 3
    
    >>> (define z '(1 2 3))
          Evaluating: (define z (quote (1 2 3)))
          Evaluating: (quote (1 2 3))
    ==> done
    
    >>> z
          Evaluating: z
    ==> (1 2 3)
    
    >>> (define y z)
          Evaluating: (define y z)
          Evaluating: z
    ==> done
    
    >>> y
          Evaluating: y
    ==> (1 2 3)
    ```
    
2.  **(Primitive) function calls:** Write evalCall(). This method should work very similarly to eval-call 
in Mini-Racket --- the only difference is that Mini-Racket's function calls always had exactly one 
argument, whereas JRacket's function calls may have any number of arguments.
    
    Once you write this function, you will be able to call the primitive (built-in, non-user-defined) 
functions in JRacket, which are defined in RacketPrimitiveFunction.java. Note that RacketPrimitiveFunction 
and RacketClosure both have an apply() method, but the one for PrimitiveFunction is already written for 
you.
    
    Hint: The code already written for you tests whether or not funcObj is a RacketFunction. If it is, you 
can cast funcObj to a RacketFunction to gain access to its apply() method.
    
    You now should be able to do the following:
    
    ```
    >>> (+ 3 3)
          Evaluating: (+ 3 3)
          Evaluating: +
    ==> 6
    
    >>> (define z 42)
          Evaluating: (define z 42)
    ==> done
    
    >>> (define q 5)
          Evaluating: (define q 5)
    ==> done
    
    >>> (* (- 2 q) z)
          Evaluating: (* (- 2 q) z)
          Evaluating: *
          Evaluating: (- 2 q)
          Evaluating: -
          Evaluating: q
          Evaluating: z
    ==> -126
    
    >>> (cons 1 '())
          Evaluating: (cons 1 (quote ()))
          Evaluating: cons
          Evaluating: (quote ())
    ==> (1)
    
    >>> (define L (cons 1 '(2 3)))
          Evaluating: (define L (cons 1 (quote (2 3))))
          Evaluating: (cons 1 (quote (2 3)))
          Evaluating: cons
          Evaluating: (quote (2 3))
    ==> done
    
    >>> L
          Evaluating: L
    ==> (1 2 3)
    
    >>> l
          Evaluating: l
    jracket.InterpreterException: Cannot find variable l
    
    >>> (= 3 4)
          Evaluating: (= 3 4)
          Evaluating: =
    ==> #f
    
    >>> (cons (car L) L)
          Evaluating: (cons (car L) L)
          Evaluating: cons
          Evaluating: (car L)
          Evaluating: car
          Evaluating: L
          Evaluating: L
    ==> (1 1 2 3)
    ```
    
    **Important: Unlike in Mini-Racket, JRacket does not need separate tests for each primitive function 
(e.g., add?, subtract?, multiply?, etc). Our interpreter _knows_ whether or not a function is a primitive 
because every object knows what class it belongs to. Therefore, as long as inside evalCall() you call 
apply() on the appropriate object, it will get dispatched correctly.**
    
3.  **Conditionals**: JRacket supports full-fledged conditionals, not like Mini-Racket's watered-down 
ifzero statement. However, your code for evalIf() should do something very similar: evaluate the test and 
see if it's equal to #t. If it is, then evaluate and return the next sub-expression, and if it isn't equal 
to #t, evaluate and return the other sub-expression.
    
    Once completed, you now should be able to do this:
    
    ```
    >>> (if (= 3 4) 1 2)
          Evaluating: (if (= 3 4) 1 2)
          Evaluating: (= 3 4)
          Evaluating: =
    ==> 2
    
    >>> (if (equal? '(1 2) '(1 2)) (cons 'a '(b)) 'kablooie)
          Evaluating: (if (equal? (quote (1 2)) (quote (1 2))) (cons (quote a) (quote (b))) (quote kablooie))
          Evaluating: (equal? (quote (1 2)) (quote (1 2)))
          Evaluating: equal?
          Evaluating: (quote (1 2))
          Evaluating: (quote (1 2))
          Evaluating: (cons (quote a) (quote (b)))
          Evaluating: cons
          Evaluating: (quote a)
          Evaluating: (quote (b))
    ==> (a b)
    ```
    
4.  **Lambdas:** JRacket supports functions that take any number of arguments. The goal of evalLambda() is 
to return a RacketClosure object that has been created from of an expression such as (lambda (x y) (+ x 
y)). Take a look at the constructor for RacketClosure. Notice how it takes a List of RacketSymbols (the 
closure's arguments), a RacketExpression (the closure's body), and the Frame corresponding to the 
environment that was current when the closure was defined. You have access to all those things inside of 
evalLambda().
    
    Hint: Creating a List of RacketSymbols filled with the argument names is a little tricky, because _you_ 
know that in a lambda expression, such as (lambda (x y) (+ x y)), the 2nd component of that expression is a 
sub-list of argument names \[e.g., (x y)\], but Java does not know that. Java just knows each component is 
itself a RacketExpression. What I suggest doing is to cast the argument list (a RacketExpression) to a 
RacketList, then you can iterate through it, get each individual variable name, and add them to a 
newly-created List of RacketSymbols.
    
    Then, finally, create a new closure of the argument names, the body of the lambda, and the environment. 
Return this new closure. (You may assume the body of the lambda has only one expression.)
    
    ```
    >>> (lambda (x y) (+ x y))
          Evaluating: (lambda (x y) (+ x y))
    ==> #[function:anonymous]
    
    >>> (lambda () 'p)
          Evaluating: (lambda () (quote p))
    ==> #[function:anonymous]
    
    >>> (lambda (lst) (cons 1 lst))
          Evaluating: (lambda (lst) (cons 1 lst))
    ==> #[function:anonymous]
    ```
    
5.  **Calling non-primitive functions:** Write apply() in RacketClosure. This will follow the same 
structure as mini-apply in Mini-Racket.
    
    Pseudocode: Make a new frame whose parent is this closure's environment (remember, apply() is a method 
inside a closure object!). Then, use defineVariable() on the new frame to bind all the arguments to their 
proper values. Then, call eval() on the body of the closure and return whatever it returns.
    
    You're done! You can now write anything:

    ```    
    >>> (define add1 (lambda (x) (+ x 1)))
          Evaluating: (define add1 (lambda (x) (+ x 1)))
          Evaluating: (lambda (x) (+ x 1))
    ==> done
    
    >>> (add1 5)
          Evaluating: (add1 5)
          Evaluating: add1
          Applying: [Func:add1 [x] ...
          Evaluating: (+ x 1)
          Evaluating: +
          Evaluating: x
    ==> 6
    
    >>> (define make-adder (lambda (x) (lambda (y) (+ x y))))
          Evaluating: (define make-adder (lambda (x) (lambda (y) (+ x y))))
          Evaluating: (lambda (x) (lambda (y) (+ x y)))
    ==> done
    
    >>> (define add2 (make-adder 2))
          Evaluating: (define add2 (make-adder 2))
          Evaluating: (make-adder 2)
          Evaluating: make-adder
          Applying: [Func:make-adder [x] ...
          Evaluating: (lambda (y) (+ x y))
    ==> done
    
    >>> (add2 19)
          Evaluating: (add2 19)
          Evaluating: add2
          Applying: [Func:add2 [y] ...
          Evaluating: (+ x y)
          Evaluating: +
          Evaluating: x
          Evaluating: y
    ==> 21
    
    >>> (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
          Evaluating: (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
          Evaluating: (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))
    ==> done
    
    >>> (fact 3)
          Evaluating: (fact 3)
          Evaluating: fact
          Applying: [Func:fact [n] ...
          Evaluating: (if (= n 0) 1 (* n (fact (- n 1))))
          Evaluating: (= n 0)
          Evaluating: =
          Evaluating: n
          Evaluating: (* n (fact (- n 1)))
          Evaluating: *
          Evaluating: n
          Evaluating: (fact (- n 1))
          Evaluating: fact
          Evaluating: (- n 1)
          Evaluating: -
          Evaluating: n
          Applying: [Func:fact [n] ...
          Evaluating: (if (= n 0) 1 (* n (fact (- n 1))))
          Evaluating: (= n 0)
          Evaluating: =
          Evaluating: n
          Evaluating: (* n (fact (- n 1)))
          Evaluating: *
          Evaluating: n
          Evaluating: (fact (- n 1))
          Evaluating: fact
          Evaluating: (- n 1)
          Evaluating: -
          Evaluating: n
          Applying: [Func:fact [n] ...
          Evaluating: (if (= n 0) 1 (* n (fact (- n 1))))
          Evaluating: (= n 0)
          Evaluating: =
          Evaluating: n
          Evaluating: (* n (fact (- n 1)))
          Evaluating: *
          Evaluating: n
          Evaluating: (fact (- n 1))
          Evaluating: fact
          Evaluating: (- n 1)
          Evaluating: -
          Evaluating: n
          Applying: [Func:fact [n] ...
          Evaluating: (if (= n 0) 1 (* n (fact (- n 1))))
          Evaluating: (= n 0)
          Evaluating: =
          Evaluating: n
    ==> 6
    ```
    

Hints
-----

*   There is a lot of code here, but it is not particularly complicated. Much of it just exists because 
Java is a more verbose language than Racket, but it's doing the same thing as our Mini-Racket interpreter.
*   There is a Utilities class provided that has two methods to convert between Java lists (i.e., 
List<RacketExpression>) and JRacket lists (i.e., '(1 2 3)). You can use these whenever you convert between 
the two types.
*   Almost every class has overridden toString(), so you can print objects for debugging purposes and you 
should see something intelligent.
*   If you want your interpreter to load anything at startup, take a look at 
addDerivedFunctionsToGlobalFrame() in Interpreter.java.

Challenges
----------

Challenge problems are designed to have little (but some) impact on your grade whether you do them or not. 
You should think of these problems as opportunities to work on something interesting and optional, rather 
than a way to raise your grade through "extra credit."

*   Enhance the define statement to permit defining functions without an explicit lambda, like (define 
(add1 x) (+ x 1)).
*   Add a let expression.
*   Add a set! expression.
*   Enhance defines and lambdas to permit more than one statement in the body.
*   Add automatic memoization of function calls, or through a special argument or something similar.
*   Add something else that you think is interesting.

What to turn in
---------------

Through Moodle, turn in all of your files. You can just upload everything, or it's probably easier to just 
make a zip file and upload that.
