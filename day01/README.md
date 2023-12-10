# Advent of Code 2023, Day 1 Solution

[Question.](https://adventofcode.com/2023/day/1)

A DCG which as part of line parsing does a forward and
backward search for the first occuring digits. This
solution is parsimonious but fairly "magical". The fwd/3
and bck/3 functions are extremely dependent on the order
of the clauses to make sure that the commitment during
backtracking happens correctly. Namely, it must be that
the predicate commits to the string prefix before it
searches for matching numbers.

Part 2 additionally considers number words and not just
digits. the fwd/bck functions are instrumented to handle
this and the variables are passed to them via the DCG
(the Fs variable).

Both parts of the solution are in *solution.prolog*. You
can use it as follows:
```
?- solve("input.txt",Part1,Part2).
```
where "input.txt" is the path to your AoC2023 input file.
