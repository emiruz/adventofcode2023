:- use_module(library(dcg/basics)).

% Simpler DCG to assign an X,Y coordinate to each character.
% when called with phrase_from_file(t(Xs),File), it results in
% a list of p(X-Y,V) terms.
ns(N,V) :- number_string(N,V).
t(_,_,[]) --> [].
t(X0,_,Xs) --> "\n",{X is X0+1},!,t(X,1,Xs).
t(X0,Y0,[p(X0-Y0,V)|Xs]) --> [V],{Y is Y0+1},!,t(X0,Y,Xs).

% Decides whether tiles are linked together in different
% directions.
link(P1,P2) :- above(P1,P2);below(P1,P2);left(P1,P2);right(P1,P2).

m(A,B) :- memberchk(A,B).

above(p(X0-Y,V0),p(X-Y,V)) :- X0-X=:=1,m(V0,`S|LJ`),m(V,`S|7F`).
below(p(X0-Y,V0),p(X-Y,V)) :- X-X0=:=1,m(V0,`S|7F`),m(V,`S|LJ`).
left(p(X-Y0,V0),p(X-Y,V))  :- Y0-Y=:=1,m(V0,`S-J7`),m(V,`S-FL`).
right(p(X-Y0,V0),p(X-Y,V)) :- Y-Y0=:=1,m(V0,`S-LF`),m(V,`S-7J`).


% Used to restrict backtracking to just candidate terms which
% could be linked to X0-Y0,
possible(p(X0-Y0,_),X-Y) :-
    X is X0+1,Y=Y0; X is X0-1,Y=Y0; X=X0,Y is Y0+1; X=X0,Y is Y0-1. 

% Follow a chain of terms until a terminal term is reached.
cycle([H|T],Term,End) :-
    possible(H,A-B), p(A-B,C), link(H,p(A-B,C)),
    \+ (T\=[],p(A-B,C)=Term),
    \+ memberchk(p(A-B,C),T),!,
    cycle([p(A-B,C),H|T],Term,End).
cycle(Acc,_,End) :- reverse(Acc,End).

% It so happens that you can work out if a point is in an enclosure
% by counting how many times it crosses a boundary when moving in one
% direction. This DCG counts horizontal boundary points for a reversed
% list.
count_([]) --> [].
count_([2|Xs]) --> (`JL`;`7F`),!,count_(Xs).
count_([1|Xs]) --> (`7L`;`JF`),!,count_(Xs).
count_([1|Xs]) --> (`|`;`L`;`7`;`F`;`J`),!, count_(Xs).
count_([0|Xs]) --> [_], count_(Xs).
count(Xs,N) :- reverse(Xs,Xs1), phrase(count_(Ns),Xs1), sumlist(Ns,N).

% Remove hyphens. Used in conjunction with count/2.
rem([])-->[].
rem(Xs)-->"-",!,rem(Xs).
rem([X|Xs])-->[X],rem(Xs).

% Check whether a term is within the boundary. It is the case if the
% count is not even.
in_bounds(p(A-B,_),Bounds) :-
    findall(K,(member(p(A-J,K),Bounds), J<B),Ks0),
    phrase(rem(Ks),Ks0),
    count(Ks,N), N mod 2 =\= 0.

% Part of making the boundary checking work is replacing S with
% its proper functional tile. This predicate checks whether a
% replacement for S would preserve the boundary cycle.
init_ok([H,N|T]) :- last(T,L), link(H,N), link(H,L).

solve(File,Part1,Part2) :-
    phrase_from_file(t(1,1,Xs),File), % Get the terms.
    retractall(p(_,_)),maplist(assertz,Xs), % Assert them .
    p(X-Y,83), cycle([p(X-Y,83)],p(X-Y,83),Bs), % Calculate boundary.
    length(Bs,N), Part1 is N/2, % Answer to part 1.
    member(Rep,`JFL7|`), Bs=[_|Rest], init_ok([p(X-Y,Rep)|Rest]), % Replace S.
    retractall(p(X-Y,_)), assertz(p(X-Y,Rep)), % Assert the S replacement.
    Bs1=[p(X-Y,Rep)|Rest], sort(Bs1,Bounds), % Sort boundary (needed by count/2)
    % Count the terms within the boundary.
    aggregate_all(count, (p(A-B,C), \+ memberchk(p(A-B,C),Bounds),
			  in_bounds(p(A-B,C),Bounds)), Part2).
