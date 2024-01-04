:- use_module(library(dcg/basics)).
:- use_module(library(clpq)).
:- use_module(library(clpfd)).

line(h(A-B-C,D-E-F)) -->
    number(A), ", ", number(B), ", ", number(C), " @",
    blanks, number(D), ",", blanks, number(E), ",", blanks, number(F).

lines([]) --> [].
lines([L|Ls]) --> line(L), "\n", lines(Ls).
lines([L]) --> line(L).

cross(Min, Max, h(Px1-Py1-_,Vx1-Vy1-_), h(Px2-Py2-_, Vx2-Vy2-_)) :-
    { (Y - Py1) / Vy1 = (X - Px1) / Vx1 },
    { (Y - Py2) / Vy2 = (X - Px2) / Vx2 },
    { X >= Min }, { X =< Max },
    { Y >= Min }, { Y =< Max }.

collide([], X-Y-Z, _) :- !, label([X,Y,Z]).
collide([h(A-B-C, D-E-F)|Hs], X-Y-Z, I-J-K) :-
    T #>= 0,
    A-X #= (I-D)*T,
    B-Y #= (J-E)*T,
    C-Z #= (K-F)*T,
    !, collide(Hs, X-Y-Z, I-J-K).

solve(File, Part1, Part2) :-
    phrase_from_file(lines(Ls), File),
    aggregate_all(
	count, ( member(A, Ls), member(B, Ls), A=h(A1,_), B=h(B1,_), A1 > B1,
		 cross(200000000000000, 400000000000000, A, B)), Part1),
    [I,J,K] ins -900..900,
    collide(Ls, X-Y-Z, I-J-K),
    Part2 is X+Y+Z.
