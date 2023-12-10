:- use_module(library(dcg/basics)).

line(Fs,Frs-Lst) --> string_without(`\n`,S),{fwd(S,Frs,Fs),bck(S,Lst,Fs)}.
lines(_,[]) --> [].
lines(Fs,[L|Ls]) --> line(Fs,L),`\n`,!,lines(Fs,Ls).
lines(Fs,[L]) --> line(Fs,L).

words(X,N) :-
    Cs=[`one`,`two`,`three`,`four`,`five`,`six`,`seven`,`eight`,`nine`],
    nth1(I,Cs,X),number_codes(I,[N]).
digits([X],X) :- member(X,`123456789`).

fwd(L,N,Fs) :-
    append(_,R,L), member(F,Fs), call(F,X,N),
    append(X,_,R),!.

bck(L0,N,Fs) :-
    reverse(L0,L), append(_,R,L),
    member(F,Fs), call(F,X0,N),
    reverse(X0,X), append(X,_,R),!.

solve(File,Part1,Part2) :-
    phrase_from_file(lines([digits],Ls),File),
    aggregate_all(sum(X),(member(A-B,Ls),number_codes(X,[A,B])),Part1),
    phrase_from_file(lines([words,digits],Ls2),File),
    aggregate_all(sum(X),(member(A-B,Ls2),number_codes(X,[A,B])),Part2).
