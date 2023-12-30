grid(_,_,[]) --> [].
grid(X0,_,Xs) --> "\n", { X is X0+1 }, !, grid(X,1,Xs).
grid(X0,Y0,[p(X0-Y0,V)|Xs]) --> [V],{ Y is Y0+1 }, !, grid(X0,Y,Xs).
grid(X0,Y0,Xs) --> `.`, { Y is Y0+1 }, !, grid(X0,Y,Xs).

next(X0-Y, X-Y) :- X is  1+X0, p(X-Y,_).
next(X0-Y, X-Y) :- X is -1+X0, p(X-Y,_).
next(X-Y0, X-Y) :- Y is  1+Y0, p(X-Y,_).
next(X-Y0, X-Y) :- Y is -1+Y0, p(X-Y,_).

forbid(X0-Y, X-Y) :- X is -1+X0, p(X-Y,0'v).
forbid(X-Y0, X-Y) :- Y is -1+Y0, p(X-Y,0'>).
forbid(X0-Y, X-Y) :- X is  1+X0, p(X-Y,0'^).
forbid(X-Y0, X-Y) :- Y is  1+Y0, p(X-Y,0'<).

:- table reach(_, _, max).

reach(O, 1-2, 1) :- next(1-2, O), \+forbid(1-2,O).
reach(O, X-Y, S) :-
    reach(X-Y, I-J, S0), S is S0 + 1,
    next(X-Y, O), \+forbid(X-Y, O), O \= I-J.

simplify_(Es, [e(A-B,X-Y,W)|T]) :-
    member(H, Es), (H=e(A-B,C-D,W1); H=e(C-D,A-B,W1)),
    member(Q, Es), (Q=e(C-D,X-Y,W2); Q=e(X-Y,C-D,W2)),
    A-B \= X-Y,
    aggregate_all(
	count,(member(E,Es),(E=e(C-D,_,_);E=e(_,C-D,_))), 2),
    select(H, Es, T0), select(Q, T0, T),
    W is W1+W2, !.
simplify_(_, _).
simplify(Es, Out) :-
    simplify_(Es, Ss),
    (Es=Ss->Out=Ss;simplify(Ss, Out)).

:- table dfs(_,_,max).

dfs(End, [End|_], N, N).
dfs(End, [H|T], N0, N) :-
    (e(H,X,W); e(X,H,W)), \+ memberchk(X, T),
    N1 is N0 + W, dfs(End, [X,H|T], N1, N).
dfs(Start, End, N) :- dfs(End, [Start], 0, N).

solve(File, Part1, Part2) :-
    phrase_from_file(grid(1,1, Ps0), File),
    last(Ps0, p(Mx-My0,_)), My is My0-1,
    include([p(_,V)]>>(V\=0'#), Ps0, Ps),
    retractall(p(_,_)), maplist(asserta, Ps),
    reach(Mx-My, _, Part1),
    findall(e(X-Y, I-J, 1),
	    (p(X-Y,_), next(X-Y, I-J), X-Y > I-J), Es),
    simplify(Es, Simple),
    retractall(e(_,_,_)), maplist(asserta, Simple),
    dfs(1-2, Mx-My, Part2).
