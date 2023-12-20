:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

% Parser
% --------------------------------------------------------------------
str(X,L) --> string_without(X,L).
num(N) --> number(N).

cmd(cond(V,Op,N,action(A))) -->
    str(`<>}`,V), [C],{([C]=`<`->Op=(#<);Op=(#>))}, num(N), ":", str(`,`,A).
cmd(action(A)) --> str(`}`,A).

cmds([C|Cs]) --> cmd(C), ",", !, cmds(Cs).
cmds([C]) --> cmd(C).

wf(work(L, Cmds)) --> string(L), "{", cmds(Cmds), "}".

wfs([]) --> [].
wfs([L|Ls]) --> wf(L), "\n", !, wfs(Ls).

item([X,M,A,S]) -->
    "{x=", num(X), ",m=", num(M), ",a=", num(A), ",s=", num(S), "}".

items([]) --> [].
items([I|Is]) --> item(I), "\n", !, items(Is).

% Processor
% --------------------------------------------------------------------
accept([action(`A`)|_], _) :- !.
accept([action(`R`)|_], _) :- !, fail.
accept([action(Label)|_], Item) :-
    work(Label, Cmds), !, accept(Cmds, Item).
accept([cond([R],Op,N,A)|_], Item) :-
    nth1(I, `xmas`, R), nth1(I, Item, V),
    call(Op, V, N), !, accept([A], Item).
accept([_|T], Item) :- accept(T, Item).

% Combinations
% --------------------------------------------------------------------
combos([action(`A`)|_], Acc, Acc).
combos([action(Label)|_], Acc, Result) :-
    work(Label, Cmds), combos(Cmds, Acc, Result).
combos([cond([R],Op,N,A)|T], Acc, Result) :-
    nth1(I, `xmas`, R),
    ( combos([A], [I-Op-N|Acc], Result);
      (Op=(#<)->Op2=(#>=);Op2=(#=<)),
      combos(T, [I-Op2-N|Acc], Result) ).

eval_(_, []) :- !.
eval_(Ps, [I-Op-N|T]) :-
    nth1(I, Ps, P), call(Op,P,N), !, eval_(Ps, T). 
eval(Cons, Domain) :-
    Ps = [X,M,A,S], Ps ins 1..4000,
    eval_(Ps, Cons),
    fd_size(X,Nx), fd_size(M,Nm), fd_size(A,Na), fd_size(S,Ns),
    Domain is Nx * Nm * Na * Ns.	     

% Solution
% --------------------------------------------------------------------
solve(File, Part1, Part2) :-
    phrase_from_file((wfs(Wfs), "\n", items(Items)), File),
    retractall(work(_,_)), maplist(assert, Wfs),
    work(`in`, Cmds),
    aggregate_all(sum(T), ( member(Item, Items),
			    accept(Cmds, Item),
			    sumlist(Item, T) ), Part1 ),
    aggregate_all(sum(T), ( combos(Cmds, [], Cons),
			    eval(Cons, T) ), Part2).
