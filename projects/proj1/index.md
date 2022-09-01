---
title: Project 1
parent: Projects
---

# CS 142 Project 1

You will write a program that simulates the two-player game [Nim](http://en.wikipedia.org/wiki/Nim). 
Nim is a simple game that where two players are presented with a number of piles of sticks.
The two players alternate removing sticks from one pile at a time according to certain rules. 
The goal of the game is sometimes to remove the last stick, or avoid removing the last stick.
Here are the rules for our version of the game.

The game begins with three piles, labeled A, B, and C. Pile A starts with 5 sticks, B with 4, and C with 3.
Player 1 moves first, and is allowed to remove any number of sticks (> 0) from a single pile.
Player 2 then moves, and is allowed to make a similar choice: they choose a pile, and remove any positive number of sticks from that pile.
The two players alternate turns until one player is forced to take the last stick (usually because it is the only one left). 
That player loses the game, and the other player wins.

## What you need to do
- Create a new IntelliJ project.  Inside, create a new Java class called `Nim`.  Write a `main` function inside this class that allows
  two players to play a game of Nim, starting from the 5/4/3 situation described above.
- While your program is running, it should clearly show whose turn it is, 
  the state of the piles of sticks, and prompt each player for a pile (a String) and a number of sticks to remove from that pile (an integer).
- Your program should not allow a user to enter an improper pile (only A, B, and C should be allowed), 
  nor an invalid number of sticks to remove from that pile (i.e., negative numbers should not be allowed, 
  nor a number of sticks greater than the number left in that pile).
- At the end of the game (whenever the last stick is removed), announce the winner.

## Testing your program
- You should test your program thoroughly to make sure it works.
- Your program should gracefully handle situations where either the pile is mistyped (ask for it again), 
  or the number of sticks to removed is wrong (ask for it again).
- You may assume the players will never enter a non-integer for the number of sticks to remove.

## Sample interactions

### Run 1:

```
It's player 1's turn.
Here is the board: 
A  B  C
5  4  3
Enter the pile: A
How many sticks would you like to remove from pile A? 3

It's player 2's turn.
Here is the board: 
A  B  C
2  4  3
Enter the pile: B
How many sticks would you like to remove from pile B? 3

It's player 1's turn.
Here is the board: 
A  B  C
2  1  3
Enter the pile: C
How many sticks would you like to remove from pile C? 2

It's player 2's turn.
Here is the board: 
A  B  C
2  1  1
Enter the pile: C
How many sticks would you like to remove from pile C? 1

It's player 1's turn.
Here is the board: 
A  B  C
2  1  0
Enter the pile: A
How many sticks would you like to remove from pile A? 2

It's player 2's turn.
Here is the board: 
A  B  C
0  1  0
Enter the pile: B
How many sticks would you like to remove from pile B? 1

The winner is player 1!
```

### Run 2:

```
It's player 1's turn.
Here is the board: 
A  B  C
5  4  3
Enter the pile: A
How many sticks would you like to remove from pile A? 5

It's player 2's turn.
Here is the board: 
A  B  C
0  4  3
Enter the pile: C
How many sticks would you like to remove from pile C? 6
Invalid number of sticks; please re-enter number: -2
Invalid number of sticks; please re-enter number: 0
Invalid number of sticks; please re-enter number: 2

It's player 1's turn.
Here is the board: 
A  B  C
0  4  1
Enter the pile: D
Invalid pile; please re-enter the pile: Z
Invalid pile; please re-enter the pile: B
How many sticks would you like to remove from pile B? 4

It's player 2's turn.
Here is the board: 
A  B  C
0  0  1
Enter the pile: C
How many sticks would you like to remove from pile C? 1

The winner is player 1!
```

## What to turn in

Through Canvas, turn in your `Nim.java` file.  Additionally, upload a text file answering the following questions:
- What bugs and conceptual difficulties did you encounter?  How did you overcome them?  What did you learn?
- Describe whatever help (if any) that you received.  Don't include readings, lectures, and exercises, but do 
  include any help from other sources, such as websites or people (including classmates and friends) and attribute them by name.
- Describe any serious problems you encountered while writing the program.
- Did you do any of the challenges (see below)?  If so, explain what you did.
- List any other feedback you have. Feel free to provide any feedbackon how much you learned from doing the assignment, and whether you enjoyed doing it.

## Guidelines

- You should use [good programming style](../../coding-style) when writing your program, except that you don't 
need to use functions in Java because we haven't fully learned them yet. You may use them if you so desire. 
All other style guidelines, including proper indentation and comments, should be followed.

- In particular, be sure to include a block comment at the top of your program with your name and the statement
  ``I have neither given nor received unauthorized aid on this program.''

## Hints and tips
- You may choose to represent the piles either as three separate `int`s or as an `int` array (`int[]`).
- To allow the user to enter the letter for a pile, I recommend using the `next()` function from the `Scanner` class.  
  We normally use `nextInt()`, `nextDouble()`, etc, but the one for `String`s is called simply `next()`.
  Note that `nextLine()` is also a valid function, but *can cause problems* when reading both numbers and Strings
  int the same program, so definitely use `next()` here.
- Comparing `String`s in Java is a bit tricky as well.  If you have two Strings, you shouldn't compare them with `==` or `!=`:
	```java
	String str = scanner.next();
	if (str == "A") {	// don't do this!
		statements...
	}
	```
	The code above is legal, however the comparison will sometimes work correctly and sometimes not (it will be clear later).
	
	The correct way to compare `String`s in Java for equality or inequality is:
	```java
	// compare for equality
	if (str.equals("A")) {
		statements...
	}

	// compare for inequality:
	if (!str.equals("A")) {    // the exclamation point means "not"
		statements...
	}
	```


## Challenge Problems
From time to time, I will offer "challenge problems" on assignments. These 
problems are designed to have little (but some) impact on your grade whether you do them or not. 
You should think of these problems as opportunities to work on something interesting and optional, 
rather than a way to raise your grade through "extra credit."

**Policy on challenge problems:**
- Challenge problems will typically allow you to get 2-5% of additional credit on an assignment, 
yet they will typically be much more difficult than this credit amount suggests.
- You should not attempt a challenge problem until you have finished the rest of an assignment.
- The tutors will not provide help on challenge problems. The instructor will provide minimal assistance only, 
  as these problems are optional and are designed to encourage independent thought.
- Challenge problems may be less carefully specified and less carefully calibrated for how difficult or time-consuming they are.
- If you solve a challenge problem, include a comment at the top of your program detailing what you did.

**Challenge problems for this assignment (upload in a separate file called `NimChallenge.java`; also turn in your regular `Nim.java` 
file without the challenges):**

- Let the players enter the starting number of sticks in each pile before the game starts.
- Add a computer player that plays randomly and let the user choose whether or not they want to play the computer or another (human) player.
- Have the computer player use some sort of intelligent strategy (but not cheat).
- Let the two players repeatedly play against each other. Report some statistics 
about the series of games at the end (how many wins for each player, most popular opening moves, etc).

