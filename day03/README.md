# Advent of Code 2023, Day 3 Solution

[Question.](https://adventofcode.com/2023/day/3)

The strategy is as follows:

1. Convert the maps to (X,Y,Value) triples.
2. Convert the triples into groups of digits.
3. Convert the groups of digits into (X,YStart,YEnd,Number) summaries.

Part 1 is answered by picking all the groups which are adjacent to
a non-digit coordinate.

Part 2 is answered by finding the coordinates of all stars, finding
all the groups of summaries which are adjacent to them, and if the
group has a length of 2, taking products and summing it all together.


Load the Prolog file corresponding to the part you want and run:

```
?- solve("input.txt",Answer).
```
where "input.txt" is the path to your AoC2023 input file.
