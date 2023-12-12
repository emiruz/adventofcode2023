# Advent of Code 2023, Day 12 Solution

[Question.](https://adventofcode.com/2023/day/12)

Currently only part 1 is solved, it is in part1.prolog.
It uses CLPFD and append/2 to constrain backtracking
enumerate all possibilities for any given row input.

I've noted that on part 2 the expansions increase
at a constant multiple of the previous number of
combinations. Unfortunately, even computing the
rows copied twice takes an unreasonable amount of
time (I think bruteforce on my laptop would take
about 20 hours).

I'll post part 2 as as soon as I solve it.

Use *part1.prolog* as follows:
```
?- solve("input.txt",Part1).
```
where "input.txt" is the path to your AoC2023 input
file.
