:- use_module(library(dcg/basics)).

ns(N,V) :- number_string(N,V).

t(_,_,[]) --> [].
t(X0,_,Xs) --> "\n",{X is X0+1},!,t(X,1,Xs).
t(X0,Y0,[p(X0,Y0,V)|Xs]) --> [V],{Y is Y0+1},!,t(X0,Y,Xs).

m(A,B) :- memberchk(A,B).

above(q(X0,Y,V0),q(X,Y,V)) :- X0-X=:=1,m(V0,`S|LJ`),m(V,`S|7F`).
below(q(X0,Y,V0),q(X,Y,V)) :- X-X0=:=1,m(V0,`S|7F`),m(V,`S|LJ`).
left(q(X,Y0,V0),q(X,Y,V))  :- Y0-Y=:=1,m(V0,`S-J7`),m(V,`S-FL`).
right(q(X,Y0,V0),q(X,Y,V)) :- Y-Y0=:=1,m(V0,`S-LF`),m(V,`S-7J`).

link(P1,P2) :-
    member(X,[above,below,left,right]), call(X,P1,P2).

cycle([H|T],Term,End) :-
    \+ length(T,1), link(H,Term), reverse([H|T],End),!.
cycle([H|T],Term,End) :-
    q(A,B,C), link(H,q(A,B,C)),
    \+ memberchk(q(A,B,C),[H|T]),!,
    cycle([q(A,B,C),H|T],Term,End).

connect(q(X0,Y0,63),q(X,Y,V)) :-
    memberchk(V,`.?`), once(X0\=X;Y0\=Y),
    1 >= sqrt((X-X0)^2 + (Y-Y0)^2),
    \+ (p(A,B,_), (Y0=Y,A<max(X,X0),A>min(X,X0);X0=X,B<max(Y,Y0),B>min(Y,Y0))).
connect(q(X0,Y0,46),q(X,Y,V)) :-
    memberchk(V,`.?`),
    once(X0\=X;Y0\=Y),
    1 >= sqrt((X-X0)^2 + (Y-Y0)^2).

q(X,Y,V) :- p(X,Y,V).
q(X,Y,63) :- [C]=`|`, p(X,Y1,C),  (Y is Y1+0.5; Y is Y1-0.5).
q(X,Y,63) :- [C]=`-`, p(X1,Y,C),  (X is X1+0.5; X is X1-0.5).
q(X,Y,63) :- [C]=`J`, p(X1,Y1,C), (X=X1, Y is Y1+0.5; X is X1+0.5, Y=Y1; X is X1-0.5, Y is Y1-0.5).
q(X,Y,63) :- [C]=`7`, p(X1,Y1,C), (X=X1, Y is Y1+0.5; X is X1-0.5, Y=Y1; X is X1+0.5, Y is Y1-0.5).
q(X,Y,63) :- [C]=`L`, p(X1,Y1,C), (X=X1, Y is Y1-0.5; X is X1+0.5, Y=Y1; X is X1-0.5, Y is Y1+0.5).
q(X,Y,63) :- [C]=`F`, p(X1,Y1,C), (X=X1, Y is Y1-0.5; X is X1-0.5, Y=Y1; X is X1+0.5, Y is Y1+0.5).

init_ok(X) :- (X=[];X=[_]),!.
init_ok([H,N|T]) :- last(T,L), link(H,N), link(H,L).

fill([M|Ms],Acc0,Fill) :-
    findall(q(A,B,C),
	    (q(A,B,C),connect(M,q(A,B,C)),
	     \+ memberchk(q(A,B,C),Acc0)),Matches),
    append(Matches,Acc0,AccNew), append(Matches,Ms,MsNew),
    fill(MsNew,AccNew,Fill).
fill([],Ms,Ms).

solve(File,Part1,Part2) :-
    phrase_from_file(t(1,1,Xs),File),
    retractall(p(_,_,_)),maplist(assertz,Xs),
    q(X,Y,83),cycle([q(X,Y,83)],q(X,Y,83),Bs0),
    length(Bs0,N), Part1 is N/2,
    forall((p(A,B,_), \+ memberchk(q(A,B,_),Bs0)),
	   (retractall(p(A,B,_)),assertz(p(A,B,46)))),
    member(Rep,`JFL7|`), Bs0=[_|Rest],
    init_ok([q(X,Y,Rep)|Rest]),
    retractall(p(X,Y,83)),
    assertz(p(X,Y,Rep)),
    Bs=[q(X,Y,Rep)|Rest],
    aggregate_all(count,q(_,_,46),PointCount),
    X1=1,Y1=1,q(X1,Y1,46), fill([q(X1,Y1,46)],[],Fill), % Chosen because not in boundary.
    aggregate_all(count,member(q(_,_,46),Fill),FillCount),
    Part2 is PointCount - FillCount.
