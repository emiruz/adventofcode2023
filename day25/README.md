# Advent of Code 2023, Day 25 Solution

[Question.](https://adventofcode.com/2023/day/25)

Here is a slow and ugly implementation of Karger's
algorithm. I will refactor it. Karger's algorithm
seems to be by far the easiest minimum cut
algorithm available. I'd prefer a more declarative
solution.

The solution is in *solution.prolog*. You can use
it like this:
```
?- solve("input.txt",Part1,Part2).
```
where "input.txt" is the path to your AoC2023 input
file.
