# Advent of Code 2023, Day 5 Solution

[Question.](https://adventofcode.com/2023/day/5)


I've revised this implementation a few times towards what
I think now is an optimal solution. As usual, a DCG process
the text input. I map all seeds and categories to sets
defined by ranges which the clpfd fdset abstraction makes
simple. At every step, all IDs can be expressed as a single
set. All that remains is to recursively map the set through
the categories to the end, where the infimum of the final
set is the answer.

Both parts of the solution are in *solution.prolog*. You can
use it as follows:
```
?- solve("input.txt",Part1,Part2).
```
where "input.txt" is the path to your AoC2023 input file.
