:- use_module(library(dcg/basics)).

n(N) --> number(N).

brick(b(A-B-C,D-E-F)) --> n(A),",",n(B),",",n(C),"~",n(D),",",n(E),",",n(F).
bricks([]) --> [].
bricks([B|Bs]) --> brick(B), "\n", !, bricks(Bs).
bricks([B]) --> brick(B).

points(A-B,A-C,A-X) :- between(B,C,X).
points(A-C,B-C,X-C) :- between(A,B,X).

project(Bs, b(A-B-C0,D-E-F0), b(A-B-C,D-E-F)) :-
    C is C0-1, F is F0-1, C > 0, F > 0,
    \+ (member(b(I-J-_, L-M-C), Bs),
	points(I-J, L-M, X), points(A-B, D-E, X)).

ground([], Acc, Acc) :- !.
ground([H|T], Acc, Final) :-
    append(Acc, T, Bs), project(Bs, H, New),
    append([[New], Acc, T], Q), !, ground(Q, [], Final).
ground([H|T], Acc, Final) :-
    ground(T, [H|Acc], Final).

chain(_, [], N0, N0) :- !.
chain(G, [H|T], N0, N) :-
    findall(X, (member(X,G), \+ memberchk(X,H)), G0),
    findall(X, (member(X, G0), project(G0,X,_)), Xs),
    Xs \= [], length(Xs, M),
    N1 is N0+M, !, chain(G0, [Xs|T], N1, N).
chain(G, [_|T], N0, N) :- chain(G, T, N0, N).

solve(File, Part1, Part2) :-
    phrase_from_file(bricks(Bs),File),
    ground(Bs, [], G),
    aggregate_all(
 	count, (member(B, G), select(B, G, G0),
		\+ (member(X, G0), project(G0, X, _))), Part1),
    aggregate_all(
 	sum(Count), (member(B,G), chain(G, [[B]], 0, Count)), Part2).
