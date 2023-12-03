# Advent of Code 2023, Day 3 Solution

[Question.](https://adventofcode.com/2023/day/3)

The strategy is to transform the map into X-Y0-Y-Value objects,
where X is the row and Y0/Y are the start/end columns. The digit
characters are merged during the transformation. The solutions
are just simple queries thereafter.

Both parts of the solution are in *solution.prolog*. You can use
it as follows:
```
?- solve("input.txt",Part,Answer).
```
where "input.txt" is the path to your AoC2023 input file.
