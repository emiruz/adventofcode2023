:- use_module(library(clpq)).
:- set_prolog_flag(table_space, 2_000_000_000).

solve(File, Part1, Part2) :-
    phrase_from_file(grid(1, 1, Ps0), File),
    last(Ps0, p(Mx-_,_)),
    include([p(_,V)]>>(V\=0'#), Ps0, Ps),
    retractall(p(_,_)), maplist(asserta, Ps),
    count(Mx, 64, Part1),
    Off is 26501365 mod 131,
    findall(X, (between(1,3,I), X is Off+Mx*I), Xs),
    maplist({Mx}/[X,C]>>(count(Mx, X, C),writeln(ok)), Xs, Ys),
    fit(Xs,Ys,A,B,C),
    Part2 is A*26501365^2 + B*26501365 + C.

grid(_,_,[]) --> [].
grid(X0,_,Xs) --> "\n", {X is X0+1}, !, grid(X,1,Xs).
grid(X0,Y0,[p(X0-Y0,V)|Xs]) --> [V],{Y is Y0+1}, !, grid(X0,Y,Xs).
grid(X0,Y0,Xs) --> `.`, {Y is Y0+1}, !, grid(X0,Y,Xs).

q(Mx, X-Y) :- A is (X-1) mod Mx+1, B is (Y-1) mod Mx+1, p(A-B,_).

:- table link/3.

link(X-Y,_,0) :- p(X-Y, 0'S).
link(_,_,0) :- !, fail.
link(X-Y,Mx,S0) :- S is S0-1, link(I-Y,Mx,S), X is I-1, q(Mx,X-Y).
link(X-Y,Mx,S0) :- S is S0-1, link(I-Y,Mx,S), X is I+1, q(Mx,X-Y).
link(X-Y,Mx,S0) :- S is S0-1, link(X-J,Mx,S), Y is J-1, q(Mx,X-Y).
link(X-Y,Mx,S0) :- S is S0-1, link(X-J,Mx,S), Y is J+1, q(Mx,X-Y).

count(Mx, M, N) :- aggregate_all(count, link(_,Mx, M), N).

fit([],[],_,_,_) :- !.
fit([X|Xs],[Y|Ys],A,B,C) :- {Y=A*X^2+B*X+C},!,fit(Xs,Ys,A,B,C).
