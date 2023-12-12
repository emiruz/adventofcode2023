:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

cons([]) --> [].
cons([L|Xs]) --> number(N),{length(L,N),maplist(=(1),L)},",",cons(Xs).
cons([L]) --> number(N),{length(L,N),maplist(=(1),L)}.

code([]) --> [].
code([V|Xs]) --> "?",{var(V)},!,code(Xs).
code([1|Xs]) --> "#",!,code(Xs).
code([0|Xs]) --> ".",code(Xs).

line(Count) --> code(Code)," ",cons(Cons),"\n",{solver(Code,Cons,Count)}.
lines([L|Ls]) --> line(L),lines(Ls).
lines([L]) --> line(L).

solver(Code,Cons0,Count) :-
    foldl([A,B,C]>>(append([B,[_],[A],[[0]]],C)),Cons0,[],Cons1),
    append(Cons2,[_],Cons1), append(Cons2,[_],Cons),
    foldl([A,B,C]>>(length(A,N0),C is B+N0),Cons0,0,N),
    length(Code,Len), M is Len-N,
    Code ins 0..1,
    global_cardinality(Code,[0-M,1-N]),
    aggregate_all(count,append(Cons,Code),Count).

solve(File,Part1) :-
    phrase_from_file(lines(Ls),File),
    sumlist(Ls,Part1).
