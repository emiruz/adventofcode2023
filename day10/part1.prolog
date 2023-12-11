:- use_module(library(dcg/basics)).

ns(N,V) :- number_string(N,V).

t(_,_,[]) --> [].
t(X0,_,Xs) --> "\n",{X is X0+1},!,t(X,1,Xs).
t(X0,Y0,[p(X0,Y0,V)|Xs]) --> [V],{Y is Y0+1},!,t(X0,Y,Xs).

m(A,B) :- memberchk(A,B).

above(p(X0,Y,V0),p(X,Y,V)) :- X0-X=:=1,m(V0,`S|LJ`),m(V,`S|7F`).
below(p(X0,Y,V0),p(X,Y,V)) :- X-X0=:=1,m(V0,`S|7F`),m(V,`S|LJ`).
left(p(X,Y0,V0),p(X,Y,V))  :- Y0-Y=:=1,m(V0,`S-J7`),m(V,`S-FL`).
right(p(X,Y0,V0),p(X,Y,V)) :- Y-Y0=:=1,m(V0,`S-LF`),m(V,`S-7J`).

link(P1,P2) :-
    member(X,[above,below,left,right]), call(X,P1,P2).

cycle([H|T],[H|T]) :-
    \+ length(T,1), p(A,B,83), link(H,p(A,B,83)),!.
cycle([Prev|T],End) :-
    p(A,B,C), link(Prev,p(A,B,C)),
    \+ member(p(A,B,C),[Prev|T]),!,
    cycle([p(A,B,C),Prev|T],End).

solve(File,Part1) :-
    phrase_from_file(t(1,1,Xs),File),
    retractall(p(_,_,_)),maplist(assertz,Xs),
    p(X,Y,83), cycle([p(X,Y,83)],Cycle),
    length(Cycle,N), Part1 is N/2,
    findall(_,(member(p(A,B,_),Cycle),write(A),write(" "),writeln(B)),_).
