:- use_module(library(dcg/basics)).
:- use_module(library(clpq)).

solve(File, Part1, Part2) :-
    phrase_from_file(lines(Ls), File),
    aggregate_all(
	count, ( member(A, Ls), member(B, Ls), A=h(A1,_), B=h(B1,_), A1 > B1,
		 cross(200000000000000, 400000000000000, A, B)), Part1),
    Ls=[A,B,C|_],
    bilinear(A, B, X-Y-Z, I-J-K), bilinear(B, C, X-Y-Z, I-J-K),
    Part2 is X+Y+Z.

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

cp(A-B-C,  D-E-F, X-Y-Z) :- X=B*F-C*E, Y=C*D-A*F, Z=A*E-B*D.
sub(A-B-C, E-F-G, X-Y-Z) :- X = A-E, Y = B-F, Z = C-G.
add(A-B-C, E-F-G, X-Y-Z) :- X = A+E, Y = B+F, Z = C+G.

bilinear(h(X1,V1), h(X2,V2), X-Y-Z, I-J-K) :-
    % X_hat x (V1-V2) + (X1-X2) x V_hat = X1 x V1 - X2 x V2
    sub(V1,V2,V_sub), sub(X2,X1,X_sub),
    cp(X-Y-Z, V_sub, X_cp), cp(I-J-K, X_sub, V_cp),
    cp(X1,V1,X1_cp), cp(X2,V2,X2_cp),
    add(X_cp, V_cp, A-B-C), sub(X1_cp, X2_cp, L-M-N),
    { A = L }, { B = M }, { C = N }.
