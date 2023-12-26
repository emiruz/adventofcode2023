# Advent of Code 2023, Day 21 Solution

[Question.](https://adventofcode.com/2023/day/21)

Part 1 can be stated elegantly in Prolog. The whole solution
-- including parsing -- is only 16 LOC. However, I'm stuck on
Part 2. Some facts are apparent:

1. For any given cell in the original map, its reachability
alternates in adjacent maps. This provides a means to count
occurrence without having to calculate reach explicitly since
the area of interest is massive.

2. Any cell reachable in N steps is also reachable in N + 2M
steps for any M >= 0 because you can always step back and forth
in order to burn unwanted steps.

The problem remains how to decide whether any particular cell
applies to any particular tile on the infinite map. I'll
revisit the problem later and post the solution once I've
figured it out.

Part 1 of the solution is in *solution.prolog*. You can use it
as follows:
```
?- solve("input.txt",Part1).
```
where "input.txt" is the path to your AoC2023 input
file.
