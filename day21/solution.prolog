grid(_,_,[]) --> [].
grid(X0,_,Xs) --> "\n", {X is X0+1}, !, grid(X,1,Xs).
grid(X0,Y0,[p(X0-Y0,V)|Xs]) --> [V],{Y is Y0+1}, !, grid(X0,Y,Xs).
grid(X0,Y0,Xs) --> `.`, {Y is Y0+1}, !, grid(X0,Y,Xs).

next(X0-Y, X-Y) :- X is 1+X0,  p(X-Y,_), !.
next(X0-Y, X-Y) :- X is -1+X0, p(X-Y,_), !.
next(X-Y0, X-Y) :- Y is 1+Y0,  p(X-Y,_), !.
next(X-Y0, X-Y) :- Y is -1+Y0, p(X-Y,_).

:- table link/2.

link(X-Y, 0) :- p(X-Y, 0'S), !.
link(X-Y, S0) :- S0>0, S is S0-1, !, next(X-Y, I-J), link(I-J, S).

solve(File, Part1) :-
    phrase_from_file(grid(1, 1, Ps0), File),
    include([p(_,V)]>>(V\=0'#), Ps0, Ps),
    retractall(p(_,_)), maplist(asserta, Ps),
    aggregate_all(count, (p(X-Y,_), link(X-Y,64)), Part1).
