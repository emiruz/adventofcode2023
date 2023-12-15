# Advent of Code 2023, Day 14 Solution

[Question.](https://adventofcode.com/2023/day/14)

The first part can be solved parsimoniously but
inefficiently using DCG+CHR like this:
```
:- use_module(library(dcg/basics)).
:- use_module(library(chr)).

:- set_prolog_flag(chr_toplevel_show_store, false).

t(_,_,[]) --> [].
t(X0,_,Xs) --> "\n",{ X is X0+1 }, !, t(X,1,Xs).
t(X0,Y0,[p(X0-Y0,V)|Xs]) --> [V], {Y is Y0+1}, !, t(X0,Y,Xs).

:- chr_constraint p/2.

p(X0-Y,79), p(X-Y,46) <=>  X is X0-1, X > 0 | p(X-Y,79), p(X0-Y,46).

solve(File,Part1) :-
    phrase_from_file(t(1,1,Ps), File),
    aggregate_all(max(X), member(p(X-_,_), Ps), Max),
    maplist(call, Ps),
    aggregate_all(sum(X),(find_chr_constraint(p(X0-_,79)),
			  X is Max+1-X0),Part1).
```

Part 2 is more fiddly. Here is a slow and ugly first
solution. I'll refactor it when I can.

Both parts of the solution are in *solution.prolog*.
You can use it as follows:
```
?- solve("input.txt",Part1,Part2).
```
where "input.txt" is the path to your AoC2023 input
file.
