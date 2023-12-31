:- use_module(library(dcg/basics)).

solve(File,Part1,Part2) :-
    phrase_from_file(t(1,1,Xs),File),
    retractall(p(_,_)),maplist(assertz,Xs),
    p(X-Y,83), cycle([p(X-Y,83)],p(X-Y,83),Bs),
    length(Bs,N), Part1 is N/2,
    member(Rep,`JFL7|`), Bs=[_|Rest], init_ok([p(X-Y,Rep)|Rest]),
    retractall(p(X-Y,_)), assertz(p(X-Y,Rep)),
    Bs1=[p(X-Y,Rep)|Rest], sort(Bs1,Bounds),
    aggregate_all(count, (p(A-B,C), \+ memberchk(p(A-B,C),Bounds),
			  in_bounds(p(A-B,C),Bounds)), Part2).

ns(N,V) :- number_string(N,V).
t(_,_,[]) --> [].
t(X0,_,Xs) --> "\n",{X is X0+1},!,t(X,1,Xs).
t(X0,Y0,[p(X0-Y0,V)|Xs]) --> [V],{Y is Y0+1},!,t(X0,Y,Xs).

link(P1,P2) :- above(P1,P2);below(P1,P2);left(P1,P2);right(P1,P2).

m(A,B) :- memberchk(A,B).

above(p(X0-Y,V0),p(X-Y,V)) :- X0-X=:=1,m(V0,`S|LJ`),m(V,`S|7F`).
below(p(X0-Y,V0),p(X-Y,V)) :- X-X0=:=1,m(V0,`S|7F`),m(V,`S|LJ`).
left(p(X-Y0,V0),p(X-Y,V))  :- Y0-Y=:=1,m(V0,`S-J7`),m(V,`S-FL`).
right(p(X-Y0,V0),p(X-Y,V)) :- Y-Y0=:=1,m(V0,`S-LF`),m(V,`S-7J`).

possible(p(X0-Y0,_),X-Y) :-
    X is X0+1,Y=Y0; X is X0-1,Y=Y0; X=X0,Y is Y0+1; X=X0,Y is Y0-1. 

cycle([H|T],Term,End) :-
    possible(H,A-B), p(A-B,C), link(H,p(A-B,C)),
    \+ (T\=[],p(A-B,C)=Term),
    \+ memberchk(p(A-B,C),T),!,
    cycle([p(A-B,C),H|T],Term,End).
cycle(Acc,_,End) :- reverse(Acc,End).

count_([]) --> [].
count_([2|Xs]) --> (`JL`;`7F`),!,count_(Xs).
count_([1|Xs]) --> (`7L`;`JF`),!,count_(Xs).
count_([1|Xs]) --> (`|`;`L`;`7`;`F`;`J`),!, count_(Xs).
count_([0|Xs]) --> [_], count_(Xs).
count(Xs,N) :- reverse(Xs,Xs1), phrase(count_(Ns),Xs1), sumlist(Ns,N).

rem([])-->[].
rem(Xs)-->"-",!,rem(Xs).
rem([X|Xs])-->[X],rem(Xs).

in_bounds(p(A-B,_),Bounds) :-
    findall(K,(member(p(A-J,K),Bounds), J<B),Ks0),
    phrase(rem(Ks),Ks0),
    count(Ks,N), N mod 2 =\= 0.

init_ok([H,N|T]) :- last(T,L), link(H,N), link(H,L).
