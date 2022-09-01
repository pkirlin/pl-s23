---
title: Coding Style
nav_order: 100
---

# COMP 142: Coding Style

Just like when writing in English, using good style when writing in a 
programming language is important because it helps you and other people read 
your code. Easily-readable code is important for you so you can maintain it (update 
it later), and for other people so they can figure out what it does as easily as possible, and possibly change it.

In this class, when writing in Java, you should follow these coding style guidelines:

## General style
- Use good modular design. Think carefully about the functions and data structures (variables) that you are creating before you start writing code.
- The `main()` function should not contain low-level details. In other words, the `main()` 
function should be a high-level overview of your solution, with the low-level details hidden in functions. This is the idea of abstraction.
- As a general guide, no function should be longer than a page of code. There are exceptions, of course, but these should truly be for exceptional cases.
- Use good error detection and handling. Always check return values from functions and handle errors appropriately.
- Try to keep the length of each line manageable. A single Java statement can be split 
over multiple lines anywhere where a space would be allowed. Unlike Python, no extra syntax (like a backslash) is necessary.

## Comments
- Use comments liberally throughout your code, especially to explain any complex, tricky, or easily misunderstood sections.
- Every program should begin with a comment block giving the following information:
	- Your name, the date, and the class.
	- The honor pledge saying you have neither given nor received any help you weren't supposed to.
	- A description of the program (a few sentences describing what the program does).
- Here is an example:
	```java
	/*
		Name: Phil Kirlin
		Date: 9/9/2021
		Class: CS142
		Pledge: I have neither given nor received unauthorized aid on this program. 
		Description: Skynet is a artificial intelligence system which guides
	      the global digital defense network.  It will not start a nuclear war. 
	      The user types commands to Skynet such as "launch probe" or "send 
	      Terminator back in time." The program reports back details such as 
	      the state of the Skynet system after each command, including statistics
	      about number of Terminators created, dollars spent, and nuclear wars 
	      started (should always be zero).
	*/
	```	
For every function you write (except `main`), you must include a comment immediately before the function definition line that contains:
- A description of what the function does.
- Details about the meanings of each of the parameters to the function.
- A description of the meaning of the return value of the function, if there is one.
	Here's an example:
  ```java
  /*
    This function computes the circumference of a circle.
    Parameters: r, the radius of the circle.
    Returns: the circumference of the circle.
  */
  public static double circumferenceOfCircle(double r) {
      return 2 * 3.14 * r;
  }
  ```
  
## Indentation and curly braces

- Java does not force you to indent your code, but it drastically improves readability.
- By default, the body of any if/else statement, loop, or function, should be indented 
the same number of spaces or tabs. This ensures that the lines of code that form the body 
can be easily distinguished from the rest of your program.
- Use a consistent curly brace style. You may choose to have an opening curly brace on 
the same line of code as the if/else test or function definition, or on a line by itself 
immediately following. The closing curly brace should be on a line by itself (with a few exceptions).
- You should pick an indentation style, and a style of using curly braces, and stick with it consistently within a single program.
- Two common indentation styles are:

	```java

	while (x == y) {
      something();
      somethingElse();
	}
	```
	
	and:
	
	
	```java

	while (x == y) 
	{
      something();
      somethingElse();
	}
	```
	
   I recommend using one of these, though there are some others you'll sometimes see.
- Exceptions: You may break your curly brace style in the following situations:
  - An if/else body has exactly one line of code. You may drop the curly braces here. You should still indent the single line.
  - Some people like this if/else style that merges some curly braces and the else statement:
  ```java
	if (test) {
		something();
		somethingElse();
	} else {
		aThirdThing();
		aFourthThing();
	}
	```
	
	You may use it if you like.

## Variable and function names

- Variable and function names should be chosen to reflect the meaning and/or use of the variable or function. 
  Single-character or generic names for variables (e.g., a, b, x, y, ch, num) are only appropriate when a 
   variable is used in a very limited context, and is never used outside of that context (for instance, 
   loop counter variables or iterator variables).
- Variable and function names typically begin with a lowercase letter.
- The "Java style" for variable names is to use "camel case," where the first letter of each word 
  (other than the first) is `putInUpperCase`. 
  
## The honor pledge

Because we are writing complex programs, it is not uncommon to get stuck. However, as the syllabus states, all homework assignments you complete outside of class must be entirely your own work, with the exception of help from the instructor, tutors, or other people while respecting the "Rules for Completing Assignments Independently" (see the syllabus).
I'm asking everyone to please include the honor pledge in the comments at the top of your programs indicating you have conformed to the request to work individually. (See the example above for the text to include.)
