# Advent of Code 2023, Day 12 Solution

[Question.](https://adventofcode.com/2023/day/12)

Finally solved. I think for many of us this question
is more intuitively interpretated as parsing the
grouping instruction and observing the consequences
to the string rather than the other way around.
However, only the latter will result in a predicate
which can be neatly memoised for the sort of
performance part 2 requires.

Both parts of the solution are in *solution.prolog*.
You can use it as follows:
```
?- solve("input.txt",Part1,Part2).
```
where "input.txt" is the path to your AoC2023 input
file.
