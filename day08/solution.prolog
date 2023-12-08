:- use_module(library(dcg/basics)).

dir([]) --> [].
dir([l|Xs]) --> "L",!,dir(Xs).
dir([r|Xs]) --> "R",dir(Xs).

nodes([]) --> [].
nodes([i(S-l,F),i(S-r,T)|Xs]) -->
    string(S)," = (",string(F),", ",string(T),")",blanks,!,nodes(Xs).

file(Dir,Nodes) --> dir(Dir),"\n\n",nodes(Nodes).

walk(Dir-C,Dir,C,N,Acc,N,Acc).
walk(Ins-Node,[H|T],C0,N0,Acc0,N,Sols) :-
    i(C0-H,Nxt),
    N1 is N0+1,
    append(T,[H],Dir),
    (t(C0)->Acc=[C0-N0|Acc0];Acc=Acc0),
    !,walk(Ins-Node,Dir,Nxt,N1,Acc,N,Sols).
walk(Start,Ins,L,S) :-
    walk(Ins-Start,Ins,Start,0,[],L,S).
    
solve(File,N,Sols) :-
    phrase_from_file(file(Dir,Nodes),File),
    retractall(i(_,_,_)),maplist(assertz,Nodes),
    retractall(t(_)), assertz(t(`ZZZ`)),
    walk(`AAA`,Dir,N,Sols).
