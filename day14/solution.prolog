:- use_module(library(dcg/basics)).

:- set_prolog_flag(chr_toplevel_show_store, false).

t(_,_,[]) --> [].
t(X0,_,Xs) --> "\n",{ X is X0+1 }, !, t(X,1,Xs).
t(X0,Y0,Xs) --> `.`, {Y is Y0+1}, !, t(X0,Y,Xs).
t(X0,Y0,[p(X0-Y0,V)|Xs]) --> [V], {Y is Y0+1}, !, t(X0,Y,Xs).

north([],_,_,Acc,Acc) :- !.
north([Y-I-_|Xs], M, N0, Acc, Out) :-
    (M=Y-I-> N is N0+1; N is I+1),
    !, north(Xs,Y-I,N,[N-Y|Acc],Out).
north(Fs0,Rs0,Ns) :-
    sort(Rs0,Rs), sort(Fs0,Fs1), reverse(Fs1,Fs),
    findall(Y-I-X, (member(X-Y,Rs), once(((member(I-Y,Fs);I-Y=0-Y), I<X))), Ns0),
    sort(Ns0,Ns1), north(Ns1,-1,-1,[],Ns2), sort(Ns2, Ns).

east(F0,R0,Rs) :-
    invert(R0,R1), flip(R1,R2),
    invert(F0,F1), flip(F1,F2),
    north(F2,R2,R3), flip(R3,R4), invert(R4,Rs).

south(F0,R0,Rs) :-
    flip(R0,R1), flip(F0,F1),
    north(F1,R1,R2), flip(R2,Rs).

west(F0,R0,Rs) :-
    flip(R0,R1), invert(R1,R2),
    flip(F0,F1), invert(F1,F2),
    north(F2,R2,R3), invert(R3,R4), flip(R4,Rs).

cycles(0,_,Rs,Rs) :- !.
cycles(N0,F0,R0,Out) :-
    N is N0-1,
    north(F0,R0,R1),
    west(F0,R1,R2),
    south(F0,R2,R3),
    east(F0,R3,Rs0),
    sort(Rs0,Rs),
    cycles(N,F0,Rs,Out).

invert(Ps,Ns) :- findall(X-Y, member(Y-X,Ps), Ns).
flip(Ps,Ns) :-
    Max is 100, % TODO: hardcoded.
    findall(X-Y,(member(I-Y,Ps), X is Max+1-I), Ns).

sub(A,B,C) :- findall(X,(member(X,A), \+ memberchk(X,B)), C).

cycle_length(St,_,St,A,A) :- A \= 0, !.
cycle_length(St,F0,R0,A0,N) :-
    cycles(1,F0,R0,Nxt),
    writeln(A0),
    A is A0+1, !,
    cycle_length(St,F0,Nxt,A,N).

solve(File,Part1, Part2) :-
    phrase_from_file(t(1,1, Ps), File),
    findall(X-Y, member(p(X-Y,79), Ps), R0), sort(R0, Rocks),
    findall(X-Y, member(p(X-Y,35), Ps), Fixed),
    north(Fixed, Rocks, New),
    Max is 100, % TODO: hardcoded.
    aggregate_all(sum(X), (member(X0-_,New), X is Max+1-X0), Part1),
    cycles(100, Fixed, Rocks, Stable),
    cycle_length(Stable, Fixed, Stable, 0, Cycles),
    Required is (1000000000 - 100) rem Cycles,
    (Required=0->Final=Stable; cycles(Required, Fixed, Stable, Final)),
    aggregate_all(sum(X), (member(X0-_, Final), X is Max+1-X0), Part2).
