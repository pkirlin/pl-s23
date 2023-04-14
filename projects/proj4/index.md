---
title: Project 4
parent: Projects
---

Programming Languages: Project 4
================================


Musical Streams
---------------

In this assignment, you will explore some fun and exciting streams and ways of using them.

### Downloading the Racket sound library

*   Open DrRacket.
*   Under the File menu, choose Package Manager.
*   Under the "Do What I Mean" tab, type "rsound" (no quotes) in the Package Source field.
*   Click "Update" and the sound library will be installed. Once it is done, you can close the package 
manager.

**If you get errors here**
- Try updating/re-downloading the latest version of Racket, which is 8.8.  I had 8.7 and got a bunch of 
red error messages during installation.  Updating to 8.8 fixed them.

### Testing the Racket sound library

*   In the lower half of DrRacket, type the command `(require rsound)` and press enter. This should load 
the sound library. Note that running this command might take a few seconds to load the library.  If this 
command completes with no errors, proceed to the next step. If you get red error messages here, try 
re-downloading the rsound library, or re-installing DrRacket and re-downloading the library.
*   Making sure your speakers are on and your sound level is reasonable, type the command `(play ding)` 
in the lower half of the DrRacket window. You should hear a "ding" sound. If you do, this confirms your 
sound library is installed correctly and is working!

### Troubleshooting

*   On Windows, if you get an error message that says `pa-open-stream: Invalid device`, try this: Go to 
the Edit menu and choose Preferences (the very last item). Find the "RSound" tab in the window that 
appears. From the dropdown box, change the frequency to 44100Hz from 48000Hz (or whatever it is, swap it 
to the other). Then re-run `(require rsound)` and `(play ding)`.

### Skeleton code

Download [proj4-start.rkt](proj4-start.rkt) from the class website to your own computer. This file 
defines all the stream-related function you need to get started, including stream-cons, stream-car, and 
so on. **Immediately run the file once you download it. You should get no errors.**

### Part A

1.  Define a function called `make-recursive-stream` that serves as a general-purpose stream-creation 
function. This function will create an infinite stream where the next element of the stream is always 
calculated from the previous element.
    
    `make-recursive-stream` takes two arguments: a function of one argument called `f`, and an initial 
value for the stream called `init`. `make-recursive-stream` returns a stream consisting of the elements 
`init`, `(f init)`, `(f (f init))`, and so on.
    
    Example: `(make-recursive-stream (lambda (x) (+ x 1)) 1)   ==> stream of 1, 2, 3, 4, ...`
    
    Example: `(make-recursive-stream (lambda (s) (string-append s "a")) "")   ==> stream of "", "a", 
"aa", "aaa", ...`
    
    Hint: This function might seem more complicated than it really is. Just think about the code that 
creates an infinite stream of integers:
    
    `(define (integers-from n)     (stream-cons n (integers-from (+ n 1))))`
    
    Think about how you would generalize this.
    
2.  Pascal's triangle is an infinitely large triangular structure of numbers that looks like this:

    ```    
                1
              1   1
            1   2   1
          1   3   3   1
        1   4   6   4   1
      1   5  10  10   5   1
    . . . . . etc . . . . . .
    ```
    
    One way to define the triangle is to say that the first two rows of the triangle consist of one 1 
and two 1's, respectively. Each further row is begins and ends with a 1, and each "interior" number in a 
row is the sum of the two numbers in the preceding row that are to the left and right of the number in 
question. For example, the 2 in the third row is the sum of the two 1's in the preceding row.
    
    Define an infinite stream called `pascal` where each item in the stream is a row of Pascal's 
triangle, represented as a list. (In other words, you are defining an infinite stream of finite lists.) 
Hint: Use `make-recursive-stream` by defining a function that takes a row of Pascal's triangle and 
generates the next row.
    
    **Do not do this problem using the binomial theorem or using some other definition of Pascal's 
triangle.**
    
    Example:  
    ```
    (define pascal . . . whatever you decide . . . )   
    (stream-enumerate pascal 6)   ==> '((1) (1 1) (1 2 1) (1 3 3 1) (1 4 6 4 1) (1 5 10 10 5 1))
    ```
    
3.  Define a function called `stream-flatten` that takes an infinite stream of *lists* and returns a new 
stream consisting of all the elements from the first list, followed by all the elements from the second 
list, and so on.
    
    Example: _(This assumes you've already written the Pascal's triangle part from above.)_  
    ```
    (define flatpascal (stream-flatten pascal))   
    (stream-enumerate flatpascal 20)   ==> '(1 1 1 1 2 1 1 3 3 1 1 4 6 4 1 1 5 10 10 5)
    ```
    
4.  Define a function called `stream-merge` that takes two infinite streams of numbers where each 
individual stream is in sorted order. `stream-merge` returns a new stream consisting of all the items 
from the two streams combined in sorted order. If there are duplicate numbers, they all should be kept 
in the merged stream.
    
    Example:  
    ```
    (define pow2 (make-recursive-stream (lambda (x) (* x 2)) 1))   
    (define pow3 (make-recursive-stream (lambda (x) (* x 3)) 1))   
    (stream-enumerate (stream-merge pow2 pow3) 20)   ==> '(1 1 2 3 4 8 9 16 27 32 64 81 128 243 256 512 729 1024 2048 2187)
    ```
    

### Part B

The next few questions all use streams to produce musical tones. This is a great use of streams because 
we can represent a sound by a stream of samples.

A little about how sound generation works:

Sounds are transmitted through the air (or other mediums) via sound waves. The sound for a "pure" 
musical tone can be represented by a sine wave; that is, a sine function with a specific *amplitude* and 
*frequency*. In sound, the amplitude of a wave controls how loud the sound is (larger amplitudes 
correspond to louder volumes) and the frequency of the wave (how fast the sine wave oscillates between 
the maximum and minimum values) controls the pitch of the tone (faster oscillations lead to higher 
pitches).

This project is primarily concerned with generating sine waves of different *frequencies*.  The 
frequency
of a wave is measured in hertz (Hz): the number of times per second the sine wave goes up and down 
equals the number of hertz.  For example, this is what a 3 Hz sine wave looks like:
![enter image description 
here](https://www.mpantenna.com/wp-content/uploads/2015/02/frequency-graph.webp)

A 3 Hz sine wave, would correspond to a musical tone below the range of human hearing.  Humans generally 
can perceive tones between about 20 Hz and 20,000 Hz.  For instance, the musical note "A" above middle C 
is 440 Hz.  You can listen to various frequencies here: [Youtube 
Video](https://www.youtube.com/watch?v=PAsMlDptjx8).

Here's a chart with music notes and their frequencies: 
https://mixbutton.com/mixing-articles/music-note-to-frequency-chart/

Because it is very hard to represent more sophisticated sounds (like speech) with sine waves, computers 
represent sound by **sampling** sound waves many times per second to try to approximate the true sound 
wave as closely as possible. For instance, CDs use an audio format that samples the sound wave 44,100 
times every second.  What this means is that we measure the amplitude of a sound wave 44,100 times every 
second.  Note that this is an entirely different concept than the Hertz discussed earlier.  Hertz refers 
to the construction of the sine wave itself -- how many times it cycles up and down per second.  Once 
the sine wave is constructed, we then **sample/measure** it 44,100 time per second, regardless of its 
Hertz number.  (This is confusing because we have two different things that are happening a certain 
number of "times per second," but they are different things.)

You will be producing streams that represent sounds using this sampling procedure. There is a sound 
library that comes with Racket that will let you listen to your streams as sounds. The only two 
functions that you need to know about are:

*   `(play-stream stream)` is a function that takes an infinite stream and plays the stream as a sound. 
It assumes that the stream consists of floating point numbers between -1 and 1 that represent a sound 
wave sampled 44,100 times per second. In other words, playing one second of sound uses the first 44,100 
numbers from the stream.
    
    **I would recommend restricting your amplitude values to between about +0.2 and -0.2 because +1 and 
-1 correspond to the loudest possible sound your computer can make. So keep your volume low to prevent 
blowing out your speakers.**
    
*   `(stop)` is a function that stops all sounds playing. You will need to call this because when you 
play an infinite stream, it naturally plays forever.

5.  Define a function called `make-sine-function` that takes an argument that will be the frequency of 
the sine wave.  This function should return a sine wave *function*: the returned function should be the 
sine wave of the frequency argument.
  The formula for a sine wave function is f(t) = A*sin(2 * pi * frequency * t), where A is the maximum 
amplitude of the sine wave, frequency is the frequency of the wave, and t is time in seconds.  In this 
case, you should use A = 0.2 (so as not blow out your speakers as mentioned above).  "frequency" is the 
argument to the `make-sine-function` function, and *t* is the argument to the *returned* function.
  Example:
  ```
  > (make-sine-function 1)
  #<procedure:...>
  > (define sine1 (make-sine-function 1))  <-- one Hz (cycle/sec)
  > (sine1 0) 
  0
  > (sine1 .25)
  0.2
  > (sine1 .5)
  2.4492935982947065e-17   <-- basically zero
  > (sine1 .75)
  -0.2
  > (sine1 1)
  -4.898587196589413e-17   <-- basically zero
  ```

6. Define a stream called `sampling-stream` that is a stream of numbers, starting from zero, and each 
successive number is 1/44100 higher than the previous one.  This can be done with `stream-map` or 
`make-recursive-stream`.
  Example:
  ```
  > (stream-enumerate sampling-stream 20)
  '(0 1/44100 1/22050 1/14700 1/11025 1/8820 1/7350 1/6300 2/11025 1/4900 1/4410 11/44100 1/3675 
  13/44100 1/3150 1/2940 4/11025 17/44100 1/2450 19/44100)
  ```

7. Define a function called `make-sine-stream` that takes a frequency as an argument. This function 
returns an infinite stream of floating point numbers representing a sine wave that ranges between -0.2 
and +0.2 that has the given frequency of `freq`, and is sampled 44,100 times per second. As 
an example, 
say we call `(define s (make-sine-stream 441))`. This gives us a stream of floating point numbers 
representing a sine wave that oscillates 441 times per second, sampled at 44,100 times per second. The 
first 100 numbers in the sequence will therefore represent one cycle of the wave.
   
   This should be done by combining the answers to the two previous questions.  This can be done with 
very little code by combining stream-map, a call to make-sine-function, and your `sampling-stream`.
    
    Example:  
    ```
    (define s (make-sine-stream 441))
    (stream-ref s 0) ==> 0
    (stream-ref s 25) ==> 0.2
    (stream-ref s 50) ==> 0
    (stream-ref s 75) ==> -0.2
    (stream-ref s 100) ==> 0
    (play-stream s) ; plays the tone
    ;; fun tests:
    (define c4 (make-sine-stream 261.63))
    (define e4 (make-sine-stream 329.63))
    (define g4 (make-sine-stream 392.00))
    (define chord (stream-map2 + c4 (stream-map2 + e4 g4)))
    ```
    
7.  `make-sine-stream` returns a stream that wastes a lot of memory because the sine function is 
periodic but your solution to the previous problem doesn't taking advantage of that. In other words, 
your function for question 5 creates an infinite number of cons cells, which is wasteful because the the 
infinite stream you are creating eventually repeats the sequence of floating point numbers over and over 
again. It would be a lot more memory efficient (constant memory versus infinite memory) if we could 
create a stream that reused cons cells to create a true "circular" stream in memory, rather than having 
to continuously allocate new cons cells to repeat the same sequence over and over.
    
    This is analogous to the two ways we looked at to create an infinite stream of 1s: one way actually 
made a recursively-linked list with one cons cell, and the other way made a linked list of lots of cons 
cells.
    
    **What you need to do:** Define a new function called make-circular-stream that takes a single list 
argument. What this function will do is create a circular stream consisting of all the items in the 
list, such that the cdr of the last element in the stream is a promise to point back to the first cons 
cell in the stream.
    
    This function is tricky to get right because you have to save the first cons cell in the stream that 
you create with stream-cons so that when you reach the end of the list, you can point back to it. You 
can use mutation to solve this problem.
    
    Here's how you'll know you did it right. The lines of code below create two infinite streams, each 
one alternating between 1 and -1. However, you can see by the output below that the "good" one is 
circular, and the "bad" one is not. (Note how the output for the good one labels the whole stream as 
"#0" and then the last cons cell contains a promise in the cdr that references #0 again, so the whole 
thing is circular.)
    
    ```
    > (define good (make-circular-stream '(1 -1)))
    > (define bad (make-recursive-stream (lambda (x) (- x)) 1))
    > (stream-enumerate good 4)
    '(1 -1 1 -1)
    > (stream-enumerate bad 4)
    '(1 -1 1 -1)
    > good
    #0='(1 . #<promise!(-1 . #<promise!#0#>)>)
    > bad
    '(1 . #<promise!(-1 . #<promise!(1 . #<promise!(-1 . #<promise!(1 . #<promise:...>)>)>)>)>)
    ```
    

### Challenge opportunity (completing this section is optional)

*   Create a stream that represents an easily-recognizable song that is playable with the play-stream 
function. For instance, make a stream that represents a nursery rhyme like Yankee Doodle or a tune like 
Ode to Joy. (worth 5 bonus points)
*   Create a function that will create a stream for an arbitrary song specified in notes or pitches 
(worth 10 bonus points). For instance:
    
    `(define yankee-doodle (make-song-stream '(c4 c4 d4 e4 c4 e4 d4 g3 c4 c4 d4 e4 c4 c4 b3 b3 c4 c4 d4 
e4 f4 e4 d4 c4 b3 g3 a3 b3 c4 c4 c4 c4)))`  
    `(play-stream yankee-doodle)` should play Yankee Doodle.
    
    You can choose your own notation for the notes or pitches of the song. In my notation above, each 
note is a pitch and an octave. So c4 means middle C, and c5 would be one octave above. Octaves run C to 
C, so b3 is the B below middle C. You can use a different notation if you want; anything is fine (as 
long as Racket can read it). For instance, in my notation above, there is no notion of how long a note 
lasts (everything is in quarter notes). So you may want to add in note durations if you want more 
sophisticated songs.
    

Grading
-------

Solutions should be:

*   Correct
*   Written in good style, including indentation and line breaks
*   Written using features discussed in class. In particular, you must not use any mutation operations 
nor arrays (even though Racket has them).


