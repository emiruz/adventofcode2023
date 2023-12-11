# Advent of Code 2023, Day 10 Solution

[Question.](https://adventofcode.com/2023/day/10)

Part 1 is a simple DCG to convert the data to 2D
coordinates followed by some recursion. Part 2
here is a "first attempt". It solves the problem
but its a rubbish solution. I'll refactor it soon.

Taking part 1 as give above, I solved part 2 by:

1. Replacing "S" with an actual tile.

2. Homogenising all points which are not a boundary
point, so that all points are either in the boundary
or not.

2. Adding in-between (+/- 0.5) tiles so that its
possible to navigate between pipes.

4. Pick any point, get its connected component
(takes ~20 minutes). The points in that component
are either in the boundary or they are not. If
not, the boundary points are the complement of it.

Both parts of the solution are in *solution.prolog*.
You can use it as follows:

```
?- solve("input.txt",Part1,Part2).
```

where "input.txt" is the path to your AoC2023 input
file.
