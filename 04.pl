:- use_module(library(dcg/basics)).

solve(File, Part1, Part2) :-
    phrase_from_file(lines(Cards),File),
    aggregate_all(
	sum(Y), ( member(_-Ns-Ps,Cards), intersect(Ns,Ps,N),
		 Y is 2^(N-1) ), Part1),
    aggregate_all(
	sum(N), ( member(C,Cards), copied(C,Cards,N) ), Part2).

rcrd([]) --> [].
rcrd([N|Rs]) --> blanks, number(N), rcrd(Rs).

card(Id,Ns,Ps) -->
    "Card",blanks, number(Id), ":", rcrd(Ns), " |", rcrd(Ps).

lines([]) --> [].
lines([Id-Ns-Ps|Ls]) --> card(Id,Ns,Ps), "\n", lines(Ls).
lines([Id-Ns-Ps]) --> card(Id,Ns,Ps).

intersect(As,Bs,N) :-
    findall(X,(member(X,As),member(X,Bs)),Xs),
    Xs\=[],length(Xs,N).

:- table copied/3.

copied(Id0-Ns0-Ps0,Cards,Total0) :-
    intersect(Ns0,Ps0,Len),
    aggregate_all(
	sum(Total), ( member(Id-Ns-Ps,Cards),
		      Id0+Len >= Id, Id > Id0,
		      copied(Id-Ns-Ps,Cards,Total) ), Sum),
    Total0 is Sum+1, !.
copied(_,_,1).

