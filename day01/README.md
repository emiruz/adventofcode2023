# Advent of Code 2023, Day 1 Solution

[Question.](https://adventofcode.com/2023/day/1)

Part 1 is a DCG which extracts the first and last digit
from any given line. The whole file is parsed, the digits
extracted, and the numbers added up for the final answer.

Part 2 is the same except there is an additional
transformation performed prior which corrects compound
words (e.g. twone, oneight) and then converts words to
numbers.

Both parts of the solution are in *solution.prolog*. You
can use it as follows:
```
?- solve("input.txt",Part1,Part2).
```
where "input.txt" is the path to your AoC2023 input file.
