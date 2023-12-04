# Advent of Code 2023, Day 4 Solution

[Question.](https://adventofcode.com/2023/day/4)

I first parse the input file into a *CardId-Ns-Ps* structure.
Part 1 is the sum of the count of intersections between Ns and
Ps. Part 2 is the count of a depth first search for cards which
are some number ahead of each others Ids. The recursive
**copied/3** predicate is tabled given that depth first search
results in a lot of repetition.

Both parts of the solution are in *solution.prolog*. You can
use it as follows:
```
?- solve("input.txt",Part,Answer).
```
where "input.txt" is the path to your AoC2023 input file.
