:- use_module(library(dcg/basics)).

solve(File, Part1, Part2) :-
    phrase_from_file(file(Dir, Nodes), File),
    retractall(i(_,_,_)), maplist(assertz, Nodes),
    retractall(t(_)), assertz(t(`ZZZ`)),
    walk(Dir, `AAA`, 0, Part1),
    findall([X,Y,0'A], i([X,Y,0'A],_,_), Starts0),
    sort(Starts0, Starts),
    findall(t([X,Y,0'Z]), i([X,Y,0'Z],_,_), Terms0),
    sort(Terms0, Terms),
    retractall(t(_)), maplist(assertz,Terms),
    findall(N, (member(C,Starts), walk(Dir,C,0,N)), Results),
    lcm(Results, Part2).

dir([]) --> [].
dir([l|Xs]) --> "L", !, dir(Xs).
dir([r|Xs]) --> "R", dir(Xs).

nodes([]) --> [].
nodes([i(S,l,F), i(S,r,T)|Xs]) -->
    string(S)," = (",string(F),", ",string(T),")", blanks, !, nodes(Xs).

file(Dir,Nodes) --> dir(Dir), "\n\n", nodes(Nodes).

walk(_,Cur,Acc,Acc) :- t(Cur),!.
walk([H|T],Cur,Acc0,N) :-
    i(Cur,H,Nxt), Acc is Acc0+1,
    append(T,[H],Dir), !, walk(Dir,Nxt,Acc,N).

lcm_(X,Y,Z) :- Z0 is gcd(X,Y), Z is X*Y // Z0.
lcm([H|T],Z) :- foldl(lcm_,T,H,Z).
