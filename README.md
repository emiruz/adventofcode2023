# Advent of Code 2023, Day 1 Solution

[Question.](https://adventofcode.com/2023/day/1)

Part 1 is a DCG which extracts the first and last digit from
any given line. The whole file is parsed, the digits extracted,
and the numbers added up for the final answer.

Part 2 is the same except there are two additional pre-processing
steps: (a) decompound words (e.g. turn twone into twoone), and
(b) turn words into numbers (e.g. one --> 1). The rest is the
same as above.

Load the correspond Prolog file and run:

```
?- solve("input.txt",Answer).
```
where "input.txt" is the path to your AoC2023 file.
