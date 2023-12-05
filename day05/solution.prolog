:- use_module(library(dcg/basics)).
:- use_module(library(chr)).

:- set_prolog_flag(chr_toplevel_show_store, false).

% Parse seed list.
seeds([]) --> [].
seeds([s(X)|Xs]) --> blanks,number(X),seeds(Xs).
% Parse rows.
rngs(_,_,[]) --> [].
rngs(Fr,To,[c(A-B-C,Fr,To)|Xs]) --> number(A),blanks,number(B),blanks,number(C),
				    "\n",rngs(Fr,To,Xs).
rngs(Fr,To,[c(A-B-C,Fr,To)]) --> number(A),blanks,number(B),blanks,number(C).
% Parse individual category.
cat([]) --> [].
cat(Rs) --> string(Fr),"-to-",string(To)," map:\n",rngs(Fr,To,Rs).
% Parse categories.
cats([Rs|Xs]) --> cat(Rs),"\n\n",!,cats(Xs).
cats([Rs]) --> cat(Rs).
% Parse file.
file(Seeds,Cats) --> "seeds:",seeds(Seeds),"\n\n",cats(Cats).

:- chr_constraint c/3.
:- chr_constraint s/1.
:- chr_constraint q/2.

% Simpagation constraints collapse route to location. 
c(D-S-L,`seed`,To) \ s(X) <=> X>=S,S+L-1>=X | N is X+D-S,q(N,To).
s(X) <=>  find_chr_constraint(c(_,`seed`,To)), q(X,To).
c(D-S-L,From,To) \ q(X,From) <=> X>=S,S+L-1>=X | N is X+D-S, q(N,To).
q(X,From) <=> From \= `location` | find_chr_constraint(c(_,From,To)), q(X,To).

% Check if a location has a corresponding seed.
best(Ss,_,Try,`seed`) :-
    member(S-L,Ss),
    Try >= S, S+L-1 >= Try,!.
best(Ss,Cs,Try0,Name) :-
    member(c(D-S-L,Fr,Name),Cs),
    Try0 >= D, D+L-1 >= Try0,
    Try is Try0+S-D,!,
    best(Ss,Cs,Try,Fr).
best(Ss,Cs,Try,Name) :-
    memberchk(c(_,Fr,Name),Cs),
    best(Ss,Cs,Try,Fr).

pairs([],Acc,Acc) :- !.
pairs([s(A),s(B)|T],Acc,Result) :- pairs(T,[A-B|Acc],Result).

until(Ss,Cs,_,X,X) :- best(Ss,Cs,X,`location`),!.
until(Ss,Cs,Inc,X0,Y) :- X is X0+Inc, until(Ss,Cs,Inc,X,Y).

solve(File,Part1,Part2) :-
    phrase_from_file(file(Ss,Cs0),File),
    flatten(Cs0,Cs),maplist(call,Cs),maplist(call,Ss),
    findall(X,find_chr_constraint(q(X,_)),Xs),
    min_member(Part1,Xs),
    pairs(Ss,[],Seeds),
    until(Seeds,Cs,10000,0,X0),
    Start is X0-10000, until(Seeds,Cs,1,Start,Part2),!.
