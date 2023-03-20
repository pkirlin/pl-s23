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



# COMP 360, Project 3 

In this assignment, you will explore using mutation and closures to create a new data type in an 
object-oriented style, as well as implement a classic algorithm using the functional programming 
paradigm. 

## Part A 

Using the same OOP-using-closures style from the **stack** data type discussed in class earlier (see the 
code from March 9), create a **graph** data type. We use “graph” here to refer to the computer 
science/mathematical notion of a graph: a structure consisting of a set of vertices and a set of edges, 
where each edge connects two vertices. Furthermore, your graph should be *undirected* (meaning an edge 
from vertex $v$ to vertex $w$ is equivalent to an edge from $w$ to $v$), and *weighted*, meaning each 
edge has an integer weight associated with it. Weights in graphs commonly represent distances or costs. 
Vertices in the graph will be represented by symbols (that is, the Racket data type "symbol"). We 
haven't used symbols that much, but these are written in Racket as single words with a quote in front of 
them, like `'foo`.

Just like the stack example from class, your graph should use closures to simulate “private” fields in 
your graph object. You may choose whatever graph representation you prefer (e.g., adjacency lists, 
adjacency matrix, or something else), and pick whatever variables you want to store the information in 
your graph. For instance, you could use one variable to store a list of the vertices in your graph, and 
another variable to store a list of the edges in the graph, where each edge would be a list of three 
things: the starting vertex, the ending vertex, and the weight of the edge (this is what I used). 

Your graph's variables will need to be *mutated* whenever vertices or edges are added. Therefore, you 
will need to use `set!` and/or mutable pairs with `set-mcar!` and `set-mcdr!`.  We have seen how to use 
the `set!` function in class, but we haven't seen `set-mcar!` or `set-mcdr!`.  These functions are used 
to mutate just the `car` or `cdr` portion of a pair, but because mutable pairs are a distinct data type 
from regular pairs in Racket, see the notes later in this document for more information about dealing 
with this data type. To be clear, you don't need to use `set-mcar!/set-mcdr!`; you can do this whole 
project with just `set!`, but `set!` is used to mutate an entire variable, whereas `set-mcar!` and 
`set-mcdr!` are used to change just a *piece* (the car or cdr) of a pair variable.

Your data structure should support the following operations: *(All of the examples below form a single 
program, in that each example depends on the results of the previous examples.)*

- Create a new graph. The call `(make-graph)` should create a new graph object and return it. All of the 
graph's functionality should be encapsulated in the returned structure (hence, it will need to be a 
closure). 

  **Example**: `(define G (make-graph))` 
  
- Add a vertex to the graph. The new vertex is represented by a Racket symbol. You may assume the same 
vertex will not be added twice. 

  **Example**: 
  ```racket
  ((G 'add-vertex!) 'briggs) 
  ((G 'add-vertex!) 'rat) 
  ((G 'add-vertex!) 'library) 
  ((G 'add-vertex!) 'rhodes-tower)
  ```
  
- Get a list of all the vertices in a graph. Returns the empty list if there are no vertices in the 
graph. 

  **Example**: `((G 'get-vertices)) ==> '(briggs rat library rhodes-tower)` *(the order of the vertices 
above is unimportant)*

- Add an edge to the graph. You may assume there will be at most one edge connecting any two vertices in 
the graph, and the same edge will not be added twice. Once an edge is added, it cannot be removed, and 
its weight cannot be changed. You may assume the vertices given as arguments will have already been 
added to the graph, and the weight argument will be a positive integer. 

  **Example**:
  ```racket
  ((G 'add-edge!) 'briggs 'rat 3) 
  ((G 'add-edge!) 'rat 'rhodes-tower 7) 
  ((G 'add-edge!) 'library 'rhodes-tower 5) 
  ((G 'add-edge!) 'library 'briggs 4)
  ```
- Get the weight attached to an edge in the graph. Given two vertices as arguments, return the weight of 
the edge connecting them. You may assume that the vertices given have an edge connecting them in the 
graph. 

  **Example**: `((G 'get-weight) 'briggs 'rat) ==> 3`
  
- Retrieve a list of the neighboring vertices of a given vertex. You may assume the vertex argument 
exists in the graph. Return the empty list if the vertex has no edges incident to it. 

  **Example**: 
  ```racket
  ((G 'get-neighbors) 'briggs) ==> '(rat library) 
  ((G 'get-neighbors) 'rat) ==> '(briggs rhodes-tower)
  ```
   *(the order of the vertices within the returned list is unimportant)*
   
- You may add any other methods to your graph class that you would like (and that make sense for a graph 
object to have), but the ones above are the ones that will be explicitly tested. For example, you may 
wish to have a `contains-vertex?` method that tests whether a vertex is in the graph, or a `get-edges` 
method that retrieves all the edges in the graph.

## Part B 

After you've written your graph class, use it to implement Dijkstra's algorithm for finding the shortest 
path between two vertices in your graph. If you don't remember Dijkstra's algorithm from 241, here's 
some pseudocode: 
```
; Assume we’re searching graph G to find the shortest path
; from vertex "start" to vertex "finish."

; A "path" is a list of vertices along with the sum of the weights on the edges
; making up the path.

initialize "frontier" to a list containing just the degenerate path starting
at start, with sum of distances for this path = 0

while frontier is not empty:
  curpath = path with smallest sum of weights from the frontier
  remove curpath from frontier
  if curpath ends with finish, then return curpath as shortest path
  else if curpath ends with a vertex in the explored list, continue loop
  else:
    curvertex = curpath’s ending vertex
    for each edge (curvertex, w) do:
      add new path (curpath + w) to the frontier, provided w is not
        in the explored list, and w is not already in curpath (prevent cycles)
    re-sort frontier so it is ordered from shortest path to longest path
    add curvertex to the explored list
```
You must implement Dijkstra's algorithm using a functional programming style, meaning you may not use 
mutation when writing the algorithm. The only place mutation should appear in this project is in 
implementing Part A, the graph class. The algorithm should return a list containing the total distance 
from the start vertex to the finish vertex, and the list of vertices along the way. 

**Example**: `(dijkstra G 'briggs 'rhodes-tower) ==> '(9 briggs library rhodes-tower)` 

Here are some suggestions to make this project easier: 
- Choose a way to represent paths in the frontier. I suggest a list that looks like `(total-distance v1 
v2 v3...)` where the vertices are in reverse order from the start vertex. This way you can easily find 
the “current vertex” by using cadr, rather than having to look at the end of the list. 

- Use the Racket function sort to keep the frontier sorted by smallest distance. See the Racket 
reference for how to use sort. That way you'll always have access to the path with the shortest path so 
far. 

- I suggest having a helper function that takes the frontier and explored lists as arguments. That way, 
the while loop in the pseudocode becomes recursive calls to this helper function with updated frontier 
and explored lists.

- Use lots of helper functions. My solution uses helper functions that build new paths, check paths for 
cycles, check if a path ends with a vertex in the explored list, and so on. 

- `map` and `filter` come in handy here. You can use `map` to iterate over the neighbors of a vertex to 
create new paths, and `filter` to eliminate the ones that have cycles or end with a vertex in the 
explored list. 

- Make liberal use of `(display)` or `(displayln)` calls to help debugging. (Remove the calls or comment 
them out before submitting your code.)

- You can use the `(reverse)` function at the very end to reverse the path so the vertices appear in the 
right order (this will be needed if you use my “backwards path” idea from above). 

- You can compare symbols for equality using `eq?` or `equal?`. The plain old equals sign will not work, 
because that's only for numbers. 

### Another example 
```
(define G3 (make-graph)) 
((G3 'add-vertex!) 'a) 
((G3 'add-vertex!) 'b) 
((G3 'add-vertex!) 'c) 
((G3 'add-vertex!) 'd) 
((G3 'add-vertex!) 'e) 
((G3 'add-vertex!) 'f) 
((G3 'add-vertex!) 'g) 
((G3 'add-vertex!) 'h) 
((G3 'add-vertex!) 'i) 
((G3 'add-edge!) 'a 'b 2) 
((G3 'add-edge!) 'a 'd 6) 
((G3 'add-edge!) 'a 'c 3) 
((G3 'add-edge!) 'b 'd 3) 
((G3 'add-edge!) 'c 'd 6) 
((G3 'add-edge!) 'd 'e 7) 
((G3 'add-edge!) 'd 'h 3) 
((G3 'add-edge!) 'd 'i 6) 
((G3 'add-edge!) 'e 'f 6) 
((G3 'add-edge!) 'f 'g 5) 
((G3 'add-edge!) 'f 'i 3) 
((G3 'add-edge!) 'g 'h 8) 
((G3 'add-edge!) 'g 'i 3) 
((G3 'add-edge!) 'h 'i 2) 
(dijkstra G3 'a 'i) ==> '(10 a b d h i) 
```

### About mutable pairs 
In Racket, normal variables (whether they represent numbers, lists, or anything else) are technically 
mutable, though we don't often use this functionality in traditional functional programming.  However, 
for this project, it's appropriate to use mutability for creating this graph data type.

We know that once a variable is created with `define`, it can be mutated with `set!`:
```racket
(define x 3)
; (define x 4)  <--- This is illegal, x cannot be mutated with define.
(set! x 5).   ; <--- This is legal; x is now 5.
```
This also works with lists:
```racket
(define x '(1 2 3))
(set! x '(4 5 6)).   ; <--- This is legal; x is now '(4 5 6)
```
However, `set!` cannot be used to modify an individual element within a list; you have to re-create the 
whole list:
```racket
(define x '(1 2 3))
; (set! (car x) 4)  <--- Illegal, can't modify just the car of a list.
; The line above attempts to change the initial 1 of the list to a 4.
(set! x '(4 2 3))  ; This works.
```
Racket has a separate data type called a **mutable pair** that can be used to construct **mutable 
lists** (just like "regular pairs" are chained together to create "regular lists").  A mutable pair has 
a `car` portion and a `cdr` portion, just like a regular pair, although they are sometimes called `mcar` 
and `mcdr` for "mutable car" and "mutable cdr."  Whenever you want to create a list where individual 
elements can be changed without re-creating the entire list from scratch, you can use mutable pairs 
instead.  You do this by adding `(require racket/mpair)` to the top of your program, and then mutable 
pairs can be created with the `mcons` function and similar `m-` prefaced functions, like `mlist`, 
`mappend`, `mmap`, `mlength`, `mreverse`, etc.  Individual pairs within the list can then be modified 
with `set-mcar!` and `set-mcdr!`

Example:
```racket
(require racket/mpair)
(define x (mcons 1 (mcons 2 (mcons 3 '()))))  ; x is a mutable list of '(1 2 3)
(set-mcar! x 4)  ; x now the mutable list '(4 2 3)
(set-mcdr! x (mlist 7 8 9))  ; x is now the mutable list '(4 7 8 9)
``` 

You choose to use mutable pairs in this assignment if you want to (note that you don't have to, you can 
just use `set!`) if you want.

There are also the functions `mlist->list` and `list->mlist` to convert between mutable lists and 
immutable lists. 

## Assessment 
Solutions should be: 
- Correct 
- In good style, including indentation and line breaks 
- Written using features discussed in class. In particular, for Part B, you must not use any mutation 
operations except for the graph mutation functions you wrote for Part A. And truthfully, your `dijkstra` 
function itself doesn't need to call `add-edge!` or `add-vertex!` anyway – the set-up code for 
constructing the graph will make those calls. 


