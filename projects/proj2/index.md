---
title: Project 2
parent: Projects
---

# CS 142 Project 2
{: .no_toc }

1. TOC
{:toc}

You will write a program that allows the user to play a "Candy Crush Saga"-style game 
that we are calling "Gumdrop Gatherer." (Let me know if you can come up with a 
better name for this...) In the game, you are presented with a two-dimensional grid of 
gumdrops, and you must gather them in a certain way to collect as many points as possible. 
You can use the mouse to click on any gumdrop you want, as long as it has at least 
one neighboring gumdrop of the same color. At that point, all the gumdrops of that 
color are neighbors of the one you selected (and neighbors of the neighbors, etc) disappear, 
and new ones drop from the top to take their place. You earn points for every gumdrop that 
disappears.

**Alternate games**

If this game doesn't particularly interest you, you may create any two-dimensional game 
of your choice of comparable programming difficulty. See the end of this document for 
requirements and suggested games. You must get the instructor's approval before starting 
work on your project if you are not writing the Gumdrop Gatherer program.

## Game description

A video is worth a thousand words, so let's take a look at a demonstration.

<video controls>
  <source src="gumdrop-demo.mp4" type="video/mp4">
Your browser does not support the video tag.
</video>


Your game doesn't have to work exactly like mine, but it must meet the following requirements:

- The game should ask the user how big the board should be, in rows and columns.
- The game should ask the user how many points to play to.
- The game should draw a board of the specified size, filled with colored circles 
representing gumdrops. I find that using four different colors for gumdrops works well.
- The game should let the user click the gumdrops (the colored circles). 
With every click, the program should check if the gumdrop clicked on has at least one 
neighbor (upper, lower, left, or right) of the same color. If it does, then all gumdrops 
connected to the original one of the same color should disappear.  Here, "connected" means
a neighbor of the same color, or neighbors of neighbors of the same color, or neighbors of
neighbors of neighbors, etc.  In other words, the single entire region of the same color
should disappear.  "Neighbors" means left, right, above, or below, not diagonal.
- When a region of gumdrops disappears, if there are any gumdrops above the region, 
then they should fall as far as they can downward, until they hit other gumdrops in the 
same column (simulating gravity).
- Once all the gumdrops have fallen down, there will still be open areas of the board. 
These should be filled with new gumdrops. (You do not have to simulate gravity to have 
the new gumdrops "fall in" from off the top of the screen; they can just appear.)
- 10 points are accumulated for each gumdrop eliminated in a region. For example, 
if you eliminate 3 gumdrops at once, that earns 30 points. However, bonuses are given 
for larger regions of gumdrops eliminated at once. For regions at least 5 gumdrops big, 
each gumdrop earns 50 points. So a region of 5 gumdrops all disappearing at once earns 
250 points. For regions at least 10 gumdrops big, you earn 100 points per gumdrop. For 
instance, if you can eliminate 10 gumdrops at once, that earns 1000 points!
- When you reach the threshold for points entered at the beginning of the program, the game ends.

## Starter code

**Make sure you create the project in a place on your computer where you can find it!
I suggest making a new subfolder in your CS 142 projects folder.**

You can download the starter code for this assignment by creating a new IntelliJ
project from version control (VCS) and using the following URL:

`https://github.com/pkirlin/cs142-f22-proj2`

In the starter project you will find three files:

- `GumdropGatherer.java` is where you will write your code.  A few methods are already
written for you.
- `SimpleCanvas.java` is the SimpleCanvas code that you don't need to worry about.
- `BouncingBall.java` is a demonstration on using the `pause()` method in `SimpleCanvas`
to make an animation.  You will probably want to use `pause()` when you start implementing
the gravity part of the project, so you can use this as an example.

## Guide to the project

This project is set up similarly to the Tic-Tac-Toe lab.  The starter code uses a number
of familiar functions, including `handleMouseClick()` and `draw()`.  

**Representing a gumdrop board**

The gumdrop board will be represented as a 2D array of integers (`int[][]`), unlike the
tic-tac-toe board, which was a 2D array of characters (`char[][]`).  While you could use
`char`s, I think integers are easier because it allows us to represent the gumdrops
on the board like this:

  - If `board[row][col]` contains a zero, it means that square on the board is empty.
  - If `board[row][col]` contains an integer 1--4 (inclusive), that means that square
  contains a gumdrop.  The specific number 1--4 corresponds to the color of the gumdrop 
    (you can pick the colors; I used red/green/blue/yellow).
  - If `board[row][col]` contains a *negative* integer 1--4, that means that square
  contains a *marked* gumdrop.  Marked gumdrops are ones that are about to be removed,
  and are drawn in white.  (This concept is explained more in-depth later.) 
  
*In other words, just like the tic-tac-toe lab used `char`s to represent the empty spaces,
X's, and O's on the board, we will use `int`s in a similar way: zero = empty space, positive
numbers = gumdrops, negative numbers = "marked" gumdrops.*

**Drawing the board on the canvas**
In tic-tac-toe we used a square size of 100-by-100 because the game board was only 3 by 3, which
then scaled up to a 300 x 300 sized canvas.   
Because the game board for this project can be larger (it's more fun if it's larger), 
I suggest using a gumdrop size of 40-by-40 instead.  In other words, instead of multiplying
and dividing by 100, you will multiply and divide by 40.

**Marking gumdrops for removal**

When a gumdrop is clicked on, we need to figure out the region of gumdrops that will be
removed.  The way we will do this is by "marking" gumdrops for removal in an iterative
process.  When a gumdrop is first clicked on, your code should change its number to its
*negative* equivalent.  Remember that the gumdrop colors are stored internally by positive
integers 1--4.  So when a gumdrop is clicked on, it will be marked for removal by changing
its integer to the corresponding negative integer.  That is, "1" becomes "-1", "2" becomes
"-2", etc.

**Finding the region of gumdrops to remove**

Remember that when a gumdrop is clicked, the entire contiguous region of gumdrops
connected to the initially-clicked gumdrop should be removed.  This will be done in two
phases.  First, we will find the region of gumdrops to be removed by "spreading" the
marked gumdrop (now a negative number) to all neighboring gumdrops above, below, left,
and right *that are the same color as the marked gumdrop*.  We do this over and over again,
as long as there are more gumdrops to mark.  Note that this iterative spreading process
is not shown visually; the user should only see the final result after the spreading
can't go any farther.

## Functions you will write

- The game begins in the `main()` method, like all Java programs.  There are instructions
there explaining the general algorithm.  To be clear, you should not write all of your
code inside `main()`!  
- I've given you the function definitions for two other functions: `handleMouseClick` and `draw`.  I've also
given you a useful function for debugging, `printBoard`, that prints the board in text.
This is useful for debugging.
- You must write a few other functions as well.  This will help you split the project into manageable pieces:

	- `hasMatchingNeighbor(int[][] board, int row, int col)`: This function returns a `boolean` indicating
	whether or not the gum drop at (row, col) has a neighboring gumdrop of the same color.
	This function will be used in `handleMouseClick` to detect if a
	gumdrop that the player clicked on has a matching neighbor of the same color.  If
	there isn't a matching neighbor, the function will return `false`, and the game won't allow
	the user to select that gumdrop.
	
	- `spreadMarked(int[][] board)`: After "marking" the initial gumdrop by switching the number
	of the gumdrop to its negative value, this function should "spread" the negative value
	by checking all the neighbors (up/down/left/right) of any marked gumdrop and marking
	neighbors with the matching number (color).  This can be called in a loop until
	no more marking happens.  Hints are below.
	
	- `removeMarked(int[][] board)`: After all the gumdrops needing to be marked are marked,
	I used this function to remove them (turn the negative numbers into zeroes).  Remember,
	gumdrops are never "truly" removed, we just turn them back into zeroes which indicates
	empty squares.
	
	- `gravity(int[][] board)`: After removing marked gumdrops, I used this function to iteratively
	lower any "floating" gumdrops until they were resting on top of existing ones.
	
	- `calcPoints(int gumdrops)`: I used this function to calculate how many points the player
	earns for removing however many gumdrops they removed.
	
	- `fillEmptySqures(int[][] board)`: I used this function after removing marked gumdrops to 
	place new random gumdrops in the empty squares.
	
You may determine the proper parameters and return values for these functions.
	
**Javadoc comments**

In Lab 2, we learned about Javadoc comments.  You should place Javadoc comments
before any function you write in this project.  There are examples in the starter
code.
	
## Testing your program

You should test your program thoroughly to make sure it works.  You will do this by 
writing "test functions."  A test function is a function designed to test a different function,
that will run a number of test cases on the function being tested.  You can call these
test functions from main.

You must write test functions for `hasMatchingNeighbor()`, `gravity()` and `spreadMarked()`.  Additional
test functions are highly recommended, but not required.

For instance, to test `gravity()`, you might write this:

```java
public static void testGravity()
{
	int[][] board1 = 
		{ {1, 3, 2, 1},
		  {1, 0, 0, 3},
		  {2, 2, 0, 0},
		  {0, 0, 0, 1} };
	gravity(board1);
	printBoard(board1);
}
```

And then you would verify that your test code prints

```
[0, 0, 0, 0]
[1, 3, 2, 1]
[1, 0, 0, 3]
[2, 2, 0, 1]
```

## What to turn in


- Through Canvas, turn in your `GumdropGatherer.java` file.  
Additionally, upload a text file answering the following questions:

1. What bugs and conceptual difficulties did you encounter? How did you overcome them? What did you learn?
2. Describe whatever help (if any) that you received. Don’t include readings, lectures, and exercises, 
but do include any help from other sources, such as websites or people (including classmates and friends) 
and attribute them by name.
3. Describe any serious problems you encountered while writing the program.
4. Did you do any of the challenges (see below)? If so, explain what you did.
5. List any other feedback you have. Feel free to provide any feedback on how much you 
learned from doing the assignment, and whether you enjoyed doing it.

## Challenges

- Add additional functionality to the game like you might find in games like Candy Crush Saga, Angry Birds, Toon Blast, etc.
- For example, make up different kinds of "items" that can appear in squares on the board. Like a bomb that explodes neighboring squares, or a present that earns you bonus points.
- Make the same-color-neighbor concept work with diagonal directions (so regions can extend diagonally as well as horizontally or vertically).

## Hints and tips

- **Checking to see if a gumdrop is next to one of the same color**
This is a slightly tricky one because it involves checking the neighboring squares on the game board. What makes it tricky is that if the gumdrop is on the border of the board, then all four neighbors (up, down, left, right) may not exist.

- **Finding the region of gumdrops that should disappear when one is clicked**

	In the demo video above, we use the color white to show the gumdrops that are about to disappear.  To make this happen, we will use a negative numbers to represent these gumdrops.

	When a user clicks on a gumdrop, first check if the gumdrop has a neighbor of the same color.  If there is a same-colored neighbor, change the gumdrop from whatever number it is to its negative version (so change 1 to -1, 2 to -2, etc).  This indicates that it will disappear.  Then we will "spread" these negative numbers to all neighboring gumdrops that have the same color as the original one (match in number).
  
  Try writing this function:
  
  `boolean spreadMarked(int[][] board)`: This function should "spread" any negative numbers s in the board to their upper, lower, left, and right neighbors, if those neighbors have the same number as the negative number in question.  This sounds harder than it is.  To do this, use a standard nested-for loop to iterate through the board.  Whenever you find a negative number, first check to see if any of the four neighbors have the positive
  version of that number in them.  If they do, overwrite the neighbor with the negative
  number.  I suggest having this function return `true` whenever at least one cell
  was changed; that way you can call this function repeatedly until it returns `false`
  (indicating the negative region can't get any larger). 
  
  Examples:
  
  Imagine a board like this:
  
  ```
  board = { {4, 3, 2, 1},
            {1, 4, 3, 3},
            {2, 4, 4, 4},
            {3, 4, 4, 1} };
  ```
  
  Imagine the user clicks on the gumdrop at row 2 and column 1.  First, we have Java do the command `board[2][1] *= -1` which changes the board to this:
  
  ```
  board = { {4,  3, 2, 1},
            {1,  4, 3, 3},
            {2, -4, 4, 4},
            {3,  4, 4, 1} };
  ```
  
  Then we call `spread(board)` which changes the board to this:
  
  ```
  board = { {4,  3,  2, 1},
            {1, -4,  3, 3},
            {2, -4, -4, 4},
            {3, -4,  4, 1} };
  ```
  
  The function call above will return `true`, meaning at least one square was changed.  Notice how the -4 has spread to three additional squares.  Then we call `spreadMarked(board)` again.  Now the board changes to:
  
  ```
  board = { {4,  3,  2,  1},
            {1, -4,  3,  3},
            {2, -4, -4, -4},
            {3, -4, -4,  1} };
  ```
  
  and the function call returns `true`, since we changed two more 4s into -4s.  Then we would `spreadMarked(board)` one more time, but the board wouldn't change, because there are no more 4s to change into -4s.  Note that the 4 in the upper left corner doesn't change to -4 because it's not directly next to any of the -4s.  So the function returns `false` (and we can therefore stop calling it).
  
- **Replacing the negative numbers with `0`s**

	You can write a simple function that replaces all the negative numbers with zeros.  The
only reason the negatives are there in the first place was so we can draw them
in white for a split second  and then
remove them from the board by replacing them with zeros.

	I called my function `removeMarked`.  I also had it return the number of 
negative numbers replaced by zeroes, which I then used to assign the points that the user
earned.   

- **Simulating gravity**

	This, along with the `spreadMarked` function, are the two most challenging parts of the program.  I suggest writing a function called `gravity` that goes through every square of the board and looks for a situation where there is a square with a number in it, and a square with a zero below it.  The 0 indicates a blank space, so the number should be lowered into the blank space (where the zero is now), and the number replaced with a zero.  This simulates gravity dropping each gumdrop into the square below. 

	Like the function above, have the `gravity` function return `true` if any
	gumdrops were moved.  That way, you can call `gravity` over and over in a loop until it returns `false` (which means all of the gumdrops have been lowered into their final positions).

	To write this function, use the standard nested for loops, but have the row loop run *backwards*, to examine the rows from the bottom up.

  Examples:
  
  Imagine a board like this:
  
  ```
  board = { {1, 3, 2, 1},
            {1, 0, 0, 3},
            {2, 2, 0, 0},
            {0, 0, 0, 1} }
  ```

  After calling `gravity(board)`, the board will look like:
  
  ```
  board = { {0, 0, 0, 0}, 
            {1, 3, 2, 1}, 
            {1, 0, 0, 3}, 
            {2, 2, 0, 1} }
  ```
  
  and the function returns `true``, because at least one number moved.
  
  We can then call `gravity(board)` again to get:
  
  ```
  board = { {0, 0, 0, 0}, 
            {1, 0, 0, 1}, 
            {1, 3, 2, 3}, 
            {2, 2, 0, 1} }
  ```
  
  and the function returns `true`.  We call it one more time:
  
  ```
  board = { {0, 0, 0, 0}, 
            {1, 0, 0, 1}, 
            {1, 3, 0, 3}, 
            {2, 2, 2, 1} }
  ```
  
  and it returns `true`.  At this point all the pieces are as low as possible, but
  the function must be called one more time to return `false` in order to determine that.

## Other games

- If you don't like this game, you can make a different one. The requirements are that it must involve a customizable-size board, and it must involve some concept where you examine the "neighbors" of the squares on the board.
- Some ideas are: Minesweeper, Connect 4, 2048, Candy Crush, Angry Birds, ...
- You must clear your idea with me first.
