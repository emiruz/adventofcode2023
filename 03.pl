:- use_module(library(chr)).
:- set_prolog_flag(chr_toplevel_show_store, false).

solve(File, Part1, Part2) :-
    phrase_from_file(t(Xs),File),
    maplist(call,Xs),
    stage(2),
    aggregate_all(sum(N), find_chr_constraint(q(_,_,_,N)),Part1),
    stage(3),
    aggregate_all(
	sum(N),(find_chr_constraint(k(_,_,[A,B])),N is A*B),Part2). 

find(X) :- find_chr_constraint(X).

ns(N,V) :- number_string(N,V).

t(Xs) --> t(1,1,Xs).
t(_,_,[]) --> [].
t(X0,_,Xs) --> "\n",{X is X0+1},!,t(X,1,Xs).
t(X0,Y0,[p(X0,Y0,Y0,[V])|Xs]) -->
    [V0], {char_code(V,V0) }, { Y is Y0+1 },! , t(X0,Y,Xs).

:- chr_constraint p/4, q/4, k/3, stage/1.

p(_,_,_,['.']) <=> true.

p(X,Y0,Y,V),p(X,Y01,Y1,V1) <=>
    Y01-Y=:=1,
    ns(_,V),ns(_,V1) | append(V,V1,V_),p(X,Y0,Y1,V_).

adj(X1,Y1,X,Y0,Y) :- X1>=X-1,X+1>=X1,Y1>=Y0-1,Y+1>=Y1.

stage(2),p(X1,Y1,Y1,_) \ p(X,Y0,Y,V) <=>
         ns(_,V),adj(X1,Y1,X,Y0,Y) | ns(N,V),q(X,Y0,Y,N).

stage(3),p(X1,Y1,Y1,['*']) \ q(X,Y0,Y,V) <=>
	 adj(X1,Y1,X,Y0,Y) | k(X1,Y1,[V]).

stage(3) \ k(X,Y,V),k(X,Y,V1) <=> V\=V1 | append(V,V1,Vs),k(X,Y,Vs).

