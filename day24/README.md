# Advent of Code 2023, Day 24 Solution

[Question.](https://adventofcode.com/2023/day/24)

Refactored solution is less "just-so". It relies on
a linearisation of the original problem using the
cross product. An smart idea originally sourced from
[here](https://old.reddit.com/r/adventofcode/comments/18pnycy/2023_day_24_solutions/kepu26z/).

Both parts of the solution are in *solution.prolog*.
You can use them like this:
```
?- solve("input.txt",Part1,Part2).
```
where "input.txt" is the path to your AoC2023 input
file.
