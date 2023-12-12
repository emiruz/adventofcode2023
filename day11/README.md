# Advent of Code 2023, Day 11 Solution

[Question.](https://adventofcode.com/2023/day/11)

Simple DCG to produce p(X-Y,N) terms where X-Y are
coordinates and N is the galaxy number. Expansions
to the left or above a galaxy are the only ones
that effect it the coordinates can be offset by
counting empty rows and columns left/above each
galaxy. The distance between galaxies is the
sum of the absolute distances of each dimension.
The same solution is applicable to both parts but
with a different expansion.

Both parts of the solution are in *solution.prolog*.
You can use it as follows:
```
?- solve("input.txt",Part1,Part2).
```
where "input.txt" is the path to your AoC2023 input
file.
