:- use_module(library(chr)).
:- use_module(library(dcg/basics)).

:- set_prolog_flag(chr_toplevel_show_store, false).

ns(N,V) :- number_string(N,V).

% Parse input, designate X,Y coordinates to each character.
t(Xs) --> t(1,1,Xs).
t(_,_,[]) --> [].
t(X0,_,Xs) --> "\n",{X is X0+1},!,t(X,1,Xs).
t(X0,Y0,[p(X0,Y0,Y0,[V])|Xs]) --> [V0],{char_code(V,V0)},{Y is Y0+1},!,t(X0,Y,Xs).

% Predicates for the constraint store.
:- chr_constraint p/4.
:- chr_constraint q/4.
:- chr_constraint k/3.
:- chr_constraint stage/1.

% Remove dot coordinates.
p(_,_,_,['.']) <=> true.

% Merge digits.
p(X,Y0,Y,V),p(X,Y01,Y1,V1) <=>
    Y01-Y=:=1,
    ns(_,V),ns(_,V1) | append(V,V1,V_),p(X,Y0,Y1,V_).

% Check if X1,Y1 (a symbol coordinate) is adjacent to a digit span.
adj(X1,Y1,X,Y0,Y) :- X1>=X-1,X+1>=X1,Y1>=Y0-1,Y+1>=Y1.

% iff stage(2), filter for digit spans adjacent to symbols.
stage(2),p(X1,Y1,Y1,_) \ p(X,Y0,Y,V) <=>
         ns(_,V),adj(X1,Y1,X,Y0,Y) | ns(N,V),q(X,Y0,Y,N).

% iff stage(3), group digit spans around star symbols.
stage(3),p(X1,Y1,Y1,['*']) \ q(X,Y0,Y,V) <=>
	 adj(X1,Y1,X,Y0,Y) | k(X1,Y1,[V]).
% iff stage(3), aggregate star groups.
stage(3) \ k(X,Y,V),k(X,Y,V1) <=> V\=V1 | append(V,V1,Vs),k(X,Y,Vs).

solve(File,Part1,Part2) :-
    phrase_from_file(t(Xs),File), % read file and parse with t DCG.
    maplist(call,Xs), % call all predicates to put into CHR store.
    %invoke stage(2) and sum qs.
    stage(2),aggregate_all(sum(N),find_chr_constraint(q(_,_,_,N)),Part1),
    % invoke stage(3) and sum ks with just 2 values.
    stage(3),aggregate_all(sum(N),(find_chr_constraint(k(_,_,[A,B])),N is A*B),Part2). 
