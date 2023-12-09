:- use_module(library(dcg/basics)).

s(A,B,C) :- string_without(A,_,B,C).
d(A,B,C) :- digit(A,B,C).

line(A-A) --> s(`123456789`),d(A),s(`123456789\n`).
line(A-B) --> s(`123456789`),d(A),string(_),d(B),s(`123456789\n`).

lines([]) --> eol.
lines([L|Ls]) --> line(L),`\n`,!,lines(Ls).
lines([L]) --> line(L).

ns(X,C) :-
    Ns=[`twoone`,`eighttwo`,`eightthree`,`oneeight`,`threeeight`,
	`fiveeight`,`nineeight`,`sevennine`],
    Cs=[`twone`,`eightwo`,`eighthree`,`oneight`,`threeight`,`fiveight`,
	`nineight`,`sevenine`],
    nth1(I,Cs,X), nth1(I,Ns,C).
ns(X,C) :-
    Ns=[`one`,`two`,`three`,`four`,`five`,`six`,`seven`,`eight`,`nine`],
    member(X,Ns), nth1(I,Ns,X), number_codes(I,C).

tr,[] --> call(eos),!.
tr,N --> {ns(X,N)},X,!,tr.
tr,[X] --> [X],tr.

solve(File,Part1,Part2) :-
    phrase_from_file(lines(Ls),File),
    aggregate_all(sum(X),(member(A-B,Ls),number_codes(X,[A,B])),Part1),
    phrase_from_file((tr,tr,lines(Ls2)),File),
    aggregate_all(sum(X),(member(A-B,Ls2),number_codes(X,[A,B])),Part2).
