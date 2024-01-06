# Advent of Code 2023, Day 21 Solution

[Question.](https://adventofcode.com/2023/day/21)

This is a refactored version of the code. I
personally thought this was the most difficult
puzzle.

The first part is fairly simple in Prolog -- one
just has to notice that any given garden patch is
reachable in N steps if the patches adjacent to it
are reachable in N-1 steps. This enables us to set
up a dynamic programming style solution which can
heavily use memoisation to speed up computation.

The second part is more difficult to see. One simple
way to investigate the relationship between counts
and repeating tiles is to note how whether a cell
can be reached varies in adjacent tiles. Namely,
after the first adjacent tile, whether a cell can
be reached with alternate. All that remains is to
figure out how to generalise this observation to
counting over a large number of steps.

One way to do it is to study how the number of
reachable cells increase with step number at tile
increments after the immediately adjacent tiles. If
one writes down the numbers for a handful of step
sizes it can be seen that the relationship between
step number and reachable cells is quadratic.

Part 2 uses the insights above, to fit a quadratic
formula using clpq and then extrapolate it to the
full step number. The resultant solution is very
concise, but it masks the complexities involved in
discovering it.

I credit members of the SWI Prolog community and
their public consideration of this problem for
making apparent things like the quadratic relationship
used herein.

Both parts of the solution are in *solution.prolog*.
You can use it as follows:
```
?- solve("input.txt", Part1, Part2).
```
where "input.txt" is the path to your AoC2023 input
file.
