:- use_module(library(dcg/basics)).

ns(N,V) :- number_string(N,V).
puzzle(_,_,[]) --> [].
puzzle(X0,_,Xs) --> "\n",{X is X0+1},!,puzzle(X,1,Xs).
puzzle(X0,Y0,[p(X0-Y0,V)|Xs]) --> [V],{Y is Y0+1},!,puzzle(X0,Y,Xs).
puzzle(X0,Y0,Xs) --> `.`,{Y is Y0+1},!,puzzle(X0,Y,Xs).

puzzles([]) --> [].
puzzles([P|Ps]) --> puzzle(1,1,P),"\n\n",!,puzzles(Ps).
puzzles([P]) --> puzzle(1,1,P), "\n".

pali_(S,I) :-
    length(S,N), I < N/2.0, J is I,
    length(Sym,J), reverse(Sym,SymR), append([Sym,SymR,_],S).
pali_(S,I) :-
    length(S,N), N/2.0 =:= I,
    reverse(Sym,SymR), append([SymR,Sym],S), Sym \= [].
pali_(S,I) :-
    length(S,N), N/2.0 < I, J is N-I,
    length(Sym,J), reverse(Sym,SymR), append([_,SymR,Sym],S).
pali(S,Is) :-
    length(S,N), L is N-1,
    findall(I, (between(1,L,I),pali_(S,I)), Is).

row_str(Ps,I,S) :- findall(X,member(p(I-_,X),Ps),S).
col_str(Ps,I,S) :- findall(X,member(p(_-I,X),Ps),S).

sol(P,Cs,Rs) :-
    last(P,p(Rows-Cols,_)),
    findall(Is, (between(1,Rows,I), row_str(P,I,S), pali(S,Is)),[H|T]),
    findall(Is, (between(1,Cols,I), col_str(P,I,S), pali(S,Is)),[H2|T2]),
    foldl([A,B,C]>>(intersection(A,B,C)),T,H,Cs0),
    foldl([A,B,C]>>(intersection(A,B,C)),T2,H2,Rs0),
    sort(Cs0,Cs), sort(Rs0,Rs).

sub(As,Bs,Cs) :-
    findall(X,(member(X,As),\+ memberchk(X,Bs)),Cs).

smudge([p(X-Y,V)|_],P,Cs0,Rs0,Score) :-
    select(p(X-Y,V),P,P0), ([V]=`#`->[VNew]=`.`;[VNew]=`#`),
    sort([p(X-Y,VNew)|P0],P1), sol(P1,Cs,Rs),
    sub(Cs,Cs0,NewCols), sub(Rs,Rs0,NewRows),
    \+ (NewCols=[],NewRows=[]),
    sumlist(NewCols,Cols), sumlist(NewRows,Rows),
    Score is Cols + 100*Rows, !.
smudge([_|T],P,Cs0,Rs0,Score) :- !, smudge(T,P,Cs0,Rs0,Score).
smudge(P0,Score) :- sol(P0,Cs,Rs), smudge(P0,P0,Cs,Rs,Score).

solve(File,Part1,Part2) :-
    phrase_from_file(puzzles(Ps),File),
    findall(Score,
	    (member(P0,Ps),sort(P0,P),
	     sol(P,Cols,Rows),
	     sumlist(Cols,ColScore),
	     sumlist(Rows,RowScore),
	     Score is RowScore*100 + ColScore), Sols),
    sumlist(Sols,Part1),
    findall(Score, (member(P0,Ps), sort(P0,P), smudge(P,Score)), Sols2),
    sumlist(Sols2,Part2).
