:- use_module(library(dcg/basics)).
:- use_module(library(heaps)).

% Parser
% --------------------------------------------------------------------
grid(_,_,[]) --> [].
grid(X0,_,Xs) --> "\n",{X is X0+1},!,grid(X,1,Xs).
grid(X0,Y0,[p(X0-Y0,N)|Xs]) -->
    [V], {number_codes(N,[V]), Y is Y0+1}, !, grid(X0,Y,Xs).
grid(X0,Y0,Xs) --> `.`,{Y is Y0+1},!,grid(X0,Y,Xs).

% Next moves: next(+Node, -Neighbours)
% --------------------------------------------------------------------
between2(A,A,A) :- !.
between2(A,B,C) :- between(A,B,C);between(B,A,C).

loss(X0-Y,X-Y,I-_,Loss) :-
    S is X0+I, aggregate_all(sum(V),(between2(S,X,K),p(K-Y,V)),Loss).
loss(X-Y0,X-Y,_-J,Loss) :-
    S is Y0+J, aggregate_all(sum(V),(between2(S,Y,K),p(X-K,V)),Loss).

next(s(0-0,_,_,_), N) :- next(s(1-1,0-1,l,0), N).
next(s(0-0,_,_,_), N) :- !,next(s(1-1,1-0,l,0), N).
next(s(X0-Y0,I-J,Last,_), s(X-Y,I-J,n,Loss)) :-
    Last\=n, !, b_getval(range, Range), member(V, Range),
    X is X0+I*V, Y is Y0+J*V,
    p(X-Y,_), loss(X0-Y0, X-Y, I-J, Loss).
next(A,C) :- next_(A,B), next(B,C).
next_(s(X-Y,I-0,_,_), s(X-Y,0-I,l,0)).
next_(s(X-Y,0-J,_,_), s(X-Y,I-0,l,0)) :- I is -1*J.
next_(s(X-Y,0-J,_,_), s(X-Y,J-0,r,0)).
next_(s(X-Y,I-0,_,_), s(X-Y,0-J,r,0)) :- J is -1*I.

% Dijkstra
% --------------------------------------------------------------------
get_(Map, s(X-Y, I0-J0, _,_), V) :-
    I is abs(I0), J is abs(J0), ht_get(Map, X-Y-I-J, V).

put_(Map, s(X-Y, I0-J0, _,_), V) :-
    I is abs(I0), J is abs(J0), ht_put(Map, X-Y-I-J, V).

dijkstra(Stop, Heap0, Visited, Map, Final) :-
    min_of_heap(Heap0, _, Node),
    get_(Visited, Node,_),
    get_from_heap(Heap0, _, _, Heap),
    !, dijkstra(Stop, Heap, Visited, Map, Final).
dijkstra(Stop, Heap0, Visited, Map, Final) :-
    get_from_heap(Heap0, _, Node, Heap1),
    Node \= s(Stop,_, _,_),
    once(get_(Map, Node, Prev); Prev=0),
    findall(Min-Neighbour,
	    ( next(Node, Neighbour), Neighbour = s(_,_,_, Loss),
	      \+ get_(Visited, Neighbour, _),
	      once(get_(Map, Neighbour, Loss0); Loss0 = 999999),
	      Min is min(Loss + Prev, Loss0)
	    ), Neighbours),
    foldl([L-N, H0, H]>>(add_to_heap(H0,L,N,H)), Neighbours, Heap1, Heap),
    foldl([L-N, M, M]>>(put_(M, N, L)), Neighbours, Map, _),
    put_(Visited, Node, Prev),
    !, dijkstra(Stop, Heap, Visited, Map, Final).
dijkstra(_, _, _, Map, Map).
dijkstra(Stop, Map) :-
    empty_heap(Heap0), ht_new(Map0), ht_new(Visited),
    add_to_heap(Heap0, 0, s(0-0,0-0,n,0), Heap),
    dijkstra(Stop, Heap, Map0, Visited, Map).

% Solve
% --------------------------------------------------------------------
solve(File,Part1, Part2) :-
    phrase_from_file(grid(1,1,Ps),File),
    retractall(p(_,_)), maplist(asserta,Ps),
    last(Ps,p(Xmax-Ymax,_)),
    b_setval(range,[1,2,3]),
    dijkstra(Xmax-Ymax, Map),
    aggregate_all(min(V), ht_gen(Map, Xmax-Ymax-_-_, V), Part1),
    b_setval(range,[4,5,6,7,8,9,10]),
    dijkstra(Xmax-Ymax, Map2),
    aggregate_all(min(V), ht_gen(Map2, Xmax-Ymax-_-_, V), Part2).
