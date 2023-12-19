# Advent of Code 2023, Day 18 Solution

[Question.](https://adventofcode.com/2023/day/18)

This question was deeply frustrating overall. Part1
was fair, and one could solve it by drawing out the
shape in 2D coordinates and then filling the
exterior to discover the area of the interior.
Part2 drastically increases the size of the area
and makes any sort of point-wise approach intractable.

I then tried all sorts of possible ways to break down
the big shape into smaller shapes which could be
considered as their own units -- nothing worked.
Eventually I started Googling for mathematics which
may be useful for determining the area on a lattice.

I found two theorems which when put together solve
the problem trivially: 1. you can use the determinant
to calculate the area of an arbitrary polygon, and
2. you can use Pick's theorem to relate that area to
via the number of boundary points to the total number
of points in the shape.


```
?- solve("input.txt",Part1,Part2).
```
where "input.txt" is the path to your AoC2023 input
file.
