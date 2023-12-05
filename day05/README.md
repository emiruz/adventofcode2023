# Advent of Code 2023, Day 5 Solution

[Question.](https://adventofcode.com/2023/day/5)

The first part is a straightforward DCG+CHR implementation.
CHR is used to collapse the route from seed to location.

For part 2, I wrote some code to test whether a location has a
corresponding seed. I then just try all the locations in order.
Since categories and seeds have large ranges, I first use an
increment of 10000 to find roughly where the solution is and
then I pass over it again closer to the goal with a unit
increment for a precise answer.

Both parts of the solution are in *solution.prolog*. You can
use it as follows:
```
?- solve("input.txt",Part1,Part2).
```
where "input.txt" is the path to your AoC2023 input file.
