:- use_module(library(dcg/basics)).

:- set_prolog_flag(table_space, 2_000_000_000).
:- table act/3.

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

expand,[] --> call(eos),!.
expand,`\n` --> `\n`,!,expand.
expand,X --> string_without(` `,S),{unfold(5,S,`?`,E1)},
	  " ",string_without(`\n`,S2),
	  {unfold(5,S2,`,`,E2), append([E1,` `,E2],X)},expand.

unfold(1,S,_,Acc,Res) :- append(Acc,S,Res),!.
unfold(N0,S,Sep,Acc0,Res) :-
    append([S,Sep,Acc0],Acc),
    N is N0-1,!,unfold(N,S,Sep,Acc,Res).
unfold(N,S,Sep,Res) :- unfold(N,S,Sep,[],Res).

nums([X|Xs]) --> number(X), ",", nums(Xs).
nums([X]) --> number(X).

line(Count) --> string_without(` `,Code), " ", nums(Groups),
		"\n", { act(Code, Groups, Count) }.
lines([Count|Ls]) --> line(Count),lines(Ls).
lines([Count]) --> line(Count).

solve(File,Part1,Part2) :-
    phrase_from_file(lines(Cs1),File),
    sumlist(Cs1,Part1),
    phrase_from_file((expand,lines(Cs2)),File),
    sumlist(Cs2,Part2).
