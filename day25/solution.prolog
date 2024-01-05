:- use_module(library(dcg/basics)).

edges(From, [e(From,To)|Es]) -->
    string_without(`\n `, To), " ", edges(From, Es).
edges(From, [e(From,To)]) --> string_without(`\n`,To).

line(Es) --> string(From), ": ", edges(From, Es).

lines([]) --> [].
lines([L|Ls]) --> line(L), "\n", !, lines(Ls).
lines([L]) --> line(L).

node_count(Es, N) :-
    findall([A,B], member(e(A,B,_), Es), Es0),
    foldl([[X,Y],B,C]>>(C=[X,Y|B]), Es0, [], Es1),
    sort(Es1, Es2), length(Es2,N).

contract(_, [], Acc, Acc) :- !.
contract(e(A,B,_), [e(X,Y,_)|Es], Acc, Out) :-
    (A=X,B=Y;A=Y,B=X), !, contract(e(A,B,_), Es, Acc, Out).
contract(e(A,B,_), [e(X,Y,N)|Es], Acc, Out) :-
    (A=X,Q=Y; A=Y,Q=X),
    !, contract(e(A,B,_), Es, [e(Q,B,N)|Acc], Out).
contract(E, [X|Es], Acc, Out) :-
    contract(E, Es, [X|Acc], Out).

karger(Es, Out) :-
    node_count(Es, 2),
    !, findall(N, member(e(_,_,N), Es), Out).
karger(Es, Out) :-
    random_member(X, Es),
    contract(X, Es, [], Es1),
    !, karger(Es1, Out).
karger(Es, N, Out) :- karger(Es, Out), length(Out, N), !.
karger(Es, N, Out) :- karger(Es, N, Out).

connected([E|Es], [], Result) :- connected(Es, [E], Result).
connected(Es, Acc, Result) :-
    member(e(A,B,_), Acc),
    member(e(X,Y,_), Es),
    once(X=A;X=B;Y=A;Y=B),
    select(e(X,Y,_), Es, Es1),
    !,connected(Es1, [e(X,Y,_)|Acc], Result).
connected(_, Acc, Acc).

solve(File, Part1) :-
    phrase_from_file(lines(Ls0), File), flatten(Ls0, Ls),
    findall(e(A,B,I), nth1(I,Ls,e(A,B)), Es),
    node_count(Es, Total),
    karger(Es, 3, Ns),
    findall(e(A,B,N), (member(e(A,B,N), Es), \+ memberchk(N, Ns)), Es2),
    connected(Es2, [], Es3),
    node_count(Es3, SubTotal),
    Part1 is (Total-SubTotal) * SubTotal.
