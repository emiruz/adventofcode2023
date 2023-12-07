:- use_module(library(dcg/basics)).

% Convert picture cards to numbers.
to_num(V,R) :- maplist(to_num_, V, R).
to_num_(C, V) :- nth1(V, `123456789TJQKA`, C), !.

% Classify hand.
class_([X,X,X,X,X],6) :- !.
class_(V,5) :- append([_,[X,X,X,X],_],V),!.
class_(V,4) :- append([_,[X,X,X],_,[Y,Y],_],V), X\=Y,!.
class_(V,4) :- append([_,[X,X],_,[Y,Y,Y],_],V), X\=Y,!.
class_(V,3) :- append([_,[X,X,X],_],V),!.
class_(V,2) :- append([_,[X,X],_,[Y,Y],_],V), X\=Y,!.
class_(V,1) :- append([_,[X,X],_],V),!.
class_(_,0).
class(V,C)  :- \+ memberchk(0,V), msort(V,V0), class_(V0,C),!.
class(X,C0) :-
    aggregate_all(max(C),(between(2,14,N),replace(X,0,N,Y),class(Y,C)),C0).

% Replace a value in a list.
replace(_,_,[]) --> [].
replace(A,B,[B|Xs]) --> [X],{X=A},!,replace(A,B,Xs).
replace(A,B,[X|Xs]) --> [X],replace(A,B,Xs).
replace(Xs,X,Y,Out) :- phrase(replace(X,Y,Out),Xs).

% Parse input.
line(C-V) --> string_without(" ",C),blank,number(V).
file([]) --> ("";"\n").
file([C-V|Xs]) --> line(C0-V),{to_num(C0,C)},"\n",file(Xs).

solve(File,Part1,Part2) :-
    phrase_from_file(file(Cs0),File),
    findall(C0-V,(member(C-V,Cs0),class(C,K),append([K],C,C0)),Cs1),
    msort(Cs1,Cs1_),
    aggregate_all(sum(X),(nth1(I,Cs1_,_-V),X is I*V),Part1),
    findall(C0-V,(member(C-V,Cs0),
		  replace(C,11,0,C1),
		  class(C1,K),
		  append([K],C1,C0)),Cs2),
    msort(Cs2,Cs2_),
    aggregate_all(sum(X),(nth1(I,Cs2_,_-V),X is I*V),Part2).
