:- use_module(library(dcg/basics)).

cmd_hash(in(_,L,F),H) :-
    number_codes(F,Cs), append([L,`=`,Cs],S), hash(S,0,H).
cmd_hash(out(_,L),H) :-
    append([L,`-`],S), hash(S,0,H).

hash([],A,A).
hash([H|T],A0,V) :- A is (A0+H)*17 rem 256, hash(T,A,V).

cmd(in(H,L,F)) --> string_without(`=-`,L), "=", number(F), { hash(L,0,H) }.
cmd(out(H,L))  --> string_without(`=-`,L), "-", { hash(L,0,H) }.

t([C|Xs]) --> cmd(C), ",", !, t(Xs).
t([C])    --> cmd(C), "\n".

replace(X,V1,V2,Out) :- nth1(I,X,V1), nth1(I,X,_,R), nth1(I,Out,V2,R).

update(_,[]) :- !.
update(Tbl, [in(H,L,F)|T]) :-
    (ht_update(Tbl,H,Old,New)->
	 once(replace(Old,L-_,L-F,New);New=[L-F|Old]);
     ht_put(Tbl,H,[L-F])),!,
    update(Tbl,T).
update(Tbl, [out(H,L)|T]) :-
    ht_get(Tbl,H,Old),
    select(L-_,Old,New),
    ht_put(Tbl,H,New),!,
    update(Tbl,T).
update(Tbl,[_|T]) :- update(Tbl,T).

solve(File, Part1, Part2) :-
    phrase_from_file(t(Ls), File),
    findall(H, (member(X,Ls), cmd_hash(X,H)), Hs),
    sumlist(Hs, Part1),
    ht_new(Tbl), update(Tbl, Ls),
    findall(Total, (ht_gen(Tbl,K,Vs0),
		    reverse(Vs0,Vs),
		    findall(X,(nth1(I,Vs,_-V), X is (K+1)*I*V),Xs),
		    sumlist(Xs,Total)), Totals),
    sumlist(Totals, Part2).
