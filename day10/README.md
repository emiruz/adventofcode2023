# Advent of Code 2023, Day 10 Solution

[Question.](https://adventofcode.com/2023/day/10)

This is a much more effective and succinct
revision of the first solution.

Part 1 is a simple DCG to convert the data to 2D
coordinates followed by some recursion.

NB: it so happens that you can work out if a point
is in an enclosure by counting how many times it
crosses a boundary when moving in one direction.
This is the most important thing in part 2. Not
including this makes the solution many times more
difficult (see previous commit).

Both parts of the solution are in *solution.prolog*.
You can use it as follows:

```
?- solve("input.txt",Part1,Part2).
```

where "input.txt" is the path to your AoC2023 input
file.
