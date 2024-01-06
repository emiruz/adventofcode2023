:- use_module(library(dcg/basics)).

solve(File, Part1, Part2) :-
    phrase_from_file(t(1,1,1,Gs),File),
    expand(Gs,2,Es), sum_paths(Es,Part1),
    expand(Gs,1000000,Es2), sum_paths(Es2,Part2).

ns(N,V) :- number_string(N,V).
t(_,_,_,[]) --> [].
t(X0,_,N,Xs) --> "\n",{X is X0+1},!,t(X,1,N,Xs).
t(X0,Y0,N0,[p(X0-Y0,N0)|Xs]) --> `#`,{Y is Y0+1, N is N0+1},!,t(X0,Y,N,Xs).
t(X0,Y0,N,Xs) --> `.`,{Y is Y0+1},!,t(X0,Y,N,Xs).

expand(Gs,F,Es) :-
    findall(X,member(p(X-_,_),Gs),Xs), max_list(Xs,Maxx),
    findall(Y,member(p(_-Y,_),Gs),Ys), max_list(Ys,Maxy),
    findall(X,(between(1,Maxx,X), \+ memberchk(p(X-_,_),Gs)),Rows),
    findall(Y,(between(1,Maxy,Y), \+ memberchk(p(_-Y,_),Gs)),Cols),
    findall(p(I-J,V),(member(p(X-Y,V),Gs),
		      aggregate_all(count,(member(R,Rows),R<X),Offx),
		      aggregate_all(count,(member(C,Cols),C<Y),Offy),
		      I is X+Offx*(F-1), J is Y+Offy*(F-1)), Es).

sum_paths(Es,Sum) :-
    aggregate_all(sum(Total), (member(p(X-Y,V1),Es), member(p(I-J,V2),Es),
			       V1<V2, Total is abs(X-I)+abs(Y-J)),Sum).
