:- use_module(library(dcg/basics)).

line([X|Xs]) --> whites,number(X),line(Xs).
line([X]) --> whites,number(X).

lines([]) --> [].
lines([L|Ls]) --> line(L),"\n",lines(Ls).
lines([L]) --> line(L).

diffs([],[]).
diffs([_],[]).
diffs([X,Y|T],[D|Ds]) :- D is Y-X, diffs([Y|T],Ds).

back([H],[H]) :- !.
back([X|T],[P,X|T]) :-
    diffs([X|T],Ds), back(Ds,[P0|_]),
    P is X-P0.

solve(File,Part1,Part2) :-
    phrase_from_file(lines(Ls),File),
    aggregate_all(sum(P),(member(L,Ls),reverse(L,Lr),back(Lr,[P|_])),Part1),
    aggregate_all(sum(P),(member(L,Ls),back(L,[P|_])),Part2).
