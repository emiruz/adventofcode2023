# Advent of Code 2023, Day 2 Solution

[Question.](https://adventofcode.com/2023/day/2)

Part 1 is a DCG which parses a string into games. It then finds the
games which do not exceed a set of thresholds and add up their IDs.

The DCG in Part 2 is exactly the same, but the parsed records are
processed a little bit differently.

Load the Prolog file corresponding to the part you want and run:

```
?- solve("input.txt",Answer).
```
where "input.txt" is the path to your AoC2023 input file.
