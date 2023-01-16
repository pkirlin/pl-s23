# Using Racket and DrRacket 
## Overview
For the first portion of the course, we will be using the Racket language (a variant of Scheme) and the
DrRacket programming system. This document describes basic installation and usage steps sufficient for
doing your homework.

The main website for Racket-related things is [http://racket-lang.org](http://racket-lang.org). The documentation for the language itself (how functions work, etc) is [The Racket
Guide](http://docs.racket-lang.org/guide/index.html).

## Installation
All you need is the DrRacket system. You can download DrRacket for your own machine from
[http://racket-lang.org/download](http://racket-lang.org/download) and follow the installation instructions.

## First Time Set-Up
The first time you use DrRacket, check the lower left corner of the window to see if it says "determine
language from source." If it doesn’t say that, click on whatever it does say and change it to "determine
language from source." DrRacket should remember this choice in the future.

## Structure of your Racket files
- Create and save programs in the (top) "Definitions Window." 
Use the bottom window only for testing, as code you write there will not automatically be saved when you choose File -> Save Definitions.  
- Be sure to start your Racket programs with the line
`#racket`.  This tells DrRacket the language you are using. (The DrRacket IDE supports a number of different languages.)
- Use a `.rkt` file extension when you save your files.

## Using DrRacket
For the most part, DrRacket is an easy-to-use system with lots of documentation. Here are a few specific
notes related to how we will use it:
- The top editing panel, called the *definitions window*, is for defining programs.  This is where you will typically write programs that you will eventually want to save to a file.
- The bottom panel, called the *interactions window*, is for evaluating Racket expressions interactively.  What you type here is normally not saved.  This area is sometimes called a "REPL," which stands for *read-evaluate-print loop*.  Every Racket expression you type here is immediately evaluated and its result printed, unlike the definitions window, where nothing happens until you click "Run."
- Clicking the "Run" button in the toolbar evaluates the program in the definitions window, making the program’s definitions available in the interactions window.
- Often, the way we will use Racket is to write definitions in the definitions window, run the program/definitions with the "Run" button, and then evaluate some expressions interactively in the interactions window.
- Every time you click "Run," the interactions window will be cleared and the REPL starts from scratch by evaluating everything in the definitions window.  But all *previous* work in the interactions window will be lost.  
- If you want to save work in the interactions window,  do so before clicking "Run." There is also an option under
“File” for "Log Definitions and Interactions" that you can use to save not only your definitions, but
what you type in the lower half of the window as well.
- A useful shortcut in the interactions window is to use Escape-P (any number of times) to bring back up recent interactions. Moreover, this works even to bring up interactions from before the most recent "Run." That
is, even though the interactions disappeared and are not part of the current environment, you can still
use Escape-P to bring them back and then edit them or press Return to re-run them.  To go backwards and forwards through the recent commands, use Escape-P and Escape-N, or Cmd-Ctrl-Up and Cmd-Ctrl-Down on Mac.
- If you cause an infinite loop, click “Stop.”
