# Lotus Sudoku
A Haskell solver for the Lotus Sudoku puzzle

#### Puzzle Rules
Fill in all cells such that
* Every arc contains the numbers from 1 to 7
* Every ring contains the numbers from 1 to 7
* No number can be repeated in any ring or arc

#### Example
![Example](https://cloud.githubusercontent.com/assets/10108593/14727426/5fddf394-07f0-11e6-9fa3-1d3b132627d9.JPG)

#### Implementation
The quickest or simplest way to write the Lotus Sudoku solver was using brute force recursive backtracking.

###### Representation
With limited knowledge of Haskell, there was no easy and aesthetically pleasing way to represent the Lotus besides a one-dimensional array containing a list of indices. The Lotus was further broken down into the following pieces: rings, left-opening arcs, and right-opening arcs. Then, indexing was done by rings beginning from the north-most outer "petal" and traversing clockwise.

###### Solver
The general approach to solving the Lotus via brute force backtracking is to generate a list of all the possible values that can be in a particular position. The possibilities are values [1,7] that are not in the same ring, left-opening arc, and right-opening arc that contain that position.

The conditions for a particular index is:
* if index > 48 then return empty list
* if index == 0 then return [1,7] not in ring/arcs
* else return back the value

```haskell
possibleSolns :: Index -> Lotus -> Solns
possibleSolns ind lts
    | ind > 48 = []
    | lts !! ind == 0 = [1..7] \\ arcRingIndices ind
    | otherwise = [lts !! ind]
    where arcRingIndices ind' = getValues lts (getArcRings ind')
```

Then, for every nonzero position in the Lotus, the solver tries all the possible values until the Lotus is complete. The idea behind the solver is:
* if at position 48 and there is only one possible value then solved
* if at position 48 and no possible values then unsolved
* if at position 48 and there are more than one possible values then unsolved
* if at any position and no possible values then unsolved
* else keep recursing

```haskell
lotusSolver :: [Int] -> [Int]
lotusSolver lts = doSolve 0 (possibleSolns 0 lts) lts
```

#### Notes
* Initial implementations of getRing and getArc used the 'elem' method. This was undesirable since 'elem' has a complexity of O(N). Current mapping is a faster implementation - O(logN) - since it does not have to traverse the list using 'elem'. Mapping works similar to a hash table, but map is implemented as a binary search tree. Note to self: "map" is a life-saver function
* A value constructor was created initially to represent the differences between the left opening arc from the right opening arc. However, this was unnecessary because the only difference between them was the list of indices that get passed in. It became much more concise and less confusing without it.
* Haskell is very adamant with its lazy evaluation which affects the order of guards and pattern matching!
