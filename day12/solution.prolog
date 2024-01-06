:- use_module(library(dcg/basics)).
:- set_prolog_flag(table_space, 2_000_000_000).

:- table act/3.

solve(File, Part1, Part2) :-
    phrase_from_file(lines(Cs1),File),
    sumlist(Cs1,Part1),
    phrase_from_file((expand,lines(Cs2)),File),
    sumlist(Cs2,Part2).

act([63|Rest], Gs, Count) :- % ?
    act([46|Rest], Gs, Count1),
    act([35|Rest], Gs, Count2),
    Count is Count1 + Count2,!.
act([46|Rest], Gs, Count) :- % .
    act(Rest, Gs, Count),!.
act(Code,[G], 1) :-
    length(Code, G), \+ memberchk(46, Code),!.
act(Code,[G], 1) :-
    length(X, G),  append(X, Rest, Code),
    \+ memberchk(46, X), \+ memberchk(35, Rest),!.
act(Code,[G|Gs], Count) :-   % #s
    length(X,G), append([X,[Next],Rest],Code),
    \+ memberchk(46, X), Next \= 35, !,
    act(Rest,Gs,Count).
act(_,_,0).

sw(A,B,C,D) :- string_without(A,B,C,D).
copy5(S, D, Out) :- append([S,D,S,D,S,D,S,D,S], Out).

expand,[] --> call(eos),!.
expand,`\n` --> `\n`, !, expand.
expand,X --> sw(` `,S),{ copy5(S,`?`,E1) }, " ", sw(`\n`,S2),
	     { copy5(S2,`,`,E2), append([E1,` `,E2], X) }, expand.

nums([X|Xs]) --> number(X), ",", nums(Xs).
nums([X]) --> number(X).

line(Count) --> string_without(` `,Code), " ", nums(Groups),
		"\n", { act(Code, Groups, Count) }.
lines([Count|Ls]) --> line(Count),lines(Ls).
lines([Count]) --> line(Count).
