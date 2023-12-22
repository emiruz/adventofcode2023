# Advent of Code 2023, Day 20 Solution

[Question.](https://adventofcode.com/2023/day/20)


Here is a very rough solution to the problem with some
sketchy code. I aim to refactor it when I can.

Part 1 is just a simulation of the rules described in
the question. For part 2 I drew a diagram of the input
and noticed that the topology of the circuit is such
that 4 sub-graphs converge through their own conjunction
nodes, through qn (in my input, a conjunction) to rx.

I monitored the qn node. It is a conjunction so it has a
memory of the 4 sub-graphs fronted by cq, jx, tt and qz.
I recorded how many button presses it took before each of
cx, jx, tt and qz signalled “high”. It occurred on
intervals for each of them:
```
jx = 3907
qz = 3911
tt = 3931
cq = 4021
```

I then used the LCM algorithm to get the largest common
multiplier for the answer. In my case:

```
lcm_(X,Y,Z) :- gcd(X,Y,Z0), Z #= X*Y // Z0.
lcm([H|T],Z) :- foldl(lcm_,T,H,Z).

?- lcm([3907,3911,3931,4021], Out).
Out = 241528477694627.
```

See comments in code for how to use otherwise. Again, I
update this code when I have some time to encapsulate
the problem better.
