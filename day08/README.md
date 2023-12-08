# Advent of Code 2023, Day 8 Solution

[Question.](https://adventofcode.com/2023/day/8)

Part 1: a DCG to parse input, and then a bit of
recursive category walking until some terminal nodes
are simultaneously reached. I use assertz to add
nodes and terminals to the store, to make it run a
bit faster then passing lists around.

Part 2: I didn't really know how to solve this
treated purely as a computational question, but
considering it as data, I noticed that for all seeds,
the cycle length repeated. So the shortest cycle that
matches all the cycle lengths would be the largest
common multiple of them all. This is the assumption
I coded into the solution.

Both parts of the solution are in *solution.prolog*.
You can use it as follows:
```
?- solve("input.txt",Part,Answer).
```
where "input.txt" is the path to your AoC2023 input
file.
