:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

% Parse input file.

seeds([]) --> [].
seeds([c(S,0,none,seed)|Xs]) -->
    blanks,number(X),{range_to_fdset(X..X,S)},seeds(Xs).

seedz([]) --> [].
seedz([c(S,0,none,seed)|Xs]) -->
    blanks,number(X),blanks,number(L),
    {E is X+L-1, range_to_fdset(X..E,S)},seedz(Xs).

rngs(_,_,[]) --> [].
rngs(Fr,To,[c(S,O,Fr,To)|Xs]) -->
    number(D),blanks,number(X),blanks,number(L),
    {E is X+L-1,range_to_fdset(X..E,S), O is D-X},
    "\n",rngs(Fr,To,Xs).
rngs(Fr,To,[c(S,O,Fr,To)]) -->
    number(D),blanks,number(X),blanks,number(L),
    {E is X+L-1,range_to_fdset(X..E,S), O is D-X}.

cat([]) --> [].
cat(Rs) --> string(Fr),"-to-",string(To)," map:\n",rngs(Fr,To,Rs).
cats([Rs|Xs]) --> cat(Rs),"\n\n",!,cats(Xs).
cats([Rs]) --> cat(Rs).
file(F,Seeds,Cats) --> "seeds:",call(F,Seeds),"\n\n",cats(Cats).


% Map seed sets to seed location sets.

offset(S0,O,S) :- X in_set S0, Y #= X+O, fd_set(Y,S).

map_(_,[],AccS,AccD,AccS,AccD) :- !.
map_(Src,[S-O|T],AccS0,AccD0,SrcU,DstU) :-
    fdset_union(S,AccS0,AccS),
    fdset_intersection(Src,S,Int),
    (empty_fdset(Int) ->
	 AccD=AccD0;offset(Int,O,Off),fdset_union(AccD0,Off,AccD)),
    !,map_(Src,T,AccS,AccD,SrcU,DstU).
map_(Src,Dests,NewSrc) :-
    empty_fdset(E),
    map_(Src,Dests,E,E,SrcU,DstU),
    fdset_subtract(Src,SrcU,Diff),
    fdset_union(Diff,DstU,NewSrc).
map(`location`,Src,Src) :- !.
map(Fr,Src,End) :-
    once(c(_,_,Fr,To)),
    findall(S-O,c(S,O,Fr,To),Dests),
    map_(Src,Dests,NewSrc),!,
    map(To,NewSrc,End).

solve_(File,F,Answer) :- 
    phrase_from_file(file(F,Ss,Cs0),File),
    flatten([Ss,Cs0],Cs),
    retractall(c(_,_,_,_)),maplist(assertz,Cs),
    findall(S,c(S,_,none,seed),[H|T]),
    foldl([A,B,C]>>fdset_union(A,B,C),T,H,Seeds),
    map(`seed`,Seeds,Final),
    fdset_min(Final,Answer).
solve(File,Part1,Part2) :-
    solve_(File,seeds,Part1),
    solve_(File,seedz,Part2).
