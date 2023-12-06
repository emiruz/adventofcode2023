# Advent of Code 2023, Day 6 Solution

[Question.](https://adventofcode.com/2023/day/6)

Not really much programming in today's puzzle. The constraint
is that X(X-T) > D, where X is the time spent charging, T is
the total available time and D is the record distance.
Therefore, the distance travelled and time charged correspond
via a quadratic equation: X^2 - XT + D = 0. The solutions to
the equation mark the bounds of the minimum and maximum
possible charge time to be within record distance.

Both parts of the solution are in *solution.prolog*. You can
use it as follows:
```
?- solve("input.txt",Part1,Part2).
```
where "input.txt" is the path to your AoC2023 input file.
