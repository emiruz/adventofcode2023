:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

% Parser
% --------------------------------------------------------------------------
update(_, _, []) :- !. 
update(M, A, [B|Bs]) :-
    ( ht_put_new(M, B, [A]) -> true; ht_update(M, B, As, [A|As]) ),
    !, update( M, A, Bs).

sources([S|Ss]) --> string_without(`,\n`,S), ", ", !, sources(Ss).
sources([S])    --> string_without(`\n`, S).

broad(M, M2) --> "broadcaster -> ", sources(Bs),
		 { ht_put(M, `0`, bc(Bs)), update(M2, `0`, Bs) }.

conj(M, M2) --> "&", string(A), " -> ", sources(Bs),
		{ ht_put( M, A, con(Bs, []) ), update(M2, A, Bs) }.

flip(M, M2) --> "%", string(A), " -> ", sources(Bs),
		{ ht_put( M, A, flp(Bs, off) ), update(M2, A, Bs) }.

lines(_,_) --> [].
lines(M,M2) --> (broad(M,M2); conj(M,M2); flip(M,M2)), "\n", !, lines(M,M2).

% Simulator
% --------------------------------------------------------------------------
simulate(_, _, [], N ,N) :- !.
simulate(Map, Inv, [`0`-low-`0`|T], N0, N) :-
    ht_get( Map, `0`, bc(Bs) ),
    findall(`0`-low-B, member(B, Bs), Cmds),
    append(T, Cmds, Stack),
    append(N0, Cmds, N1),
    !, simulate(Map, Inv, Stack, N1, N).
simulate(Map, Inv, [_-high-A|T], N0, N) :-
    ht_get( Map, A, flp(_,_) ),
    !, simulate(Map, Inv, T, N0, N).
simulate(Map, Inv, [_-low-A|T], N0, N) :-
    ht_get( Map, A, flp(Bs, S0) ),
    (S0=off->S=on; S=off), (S=on->P=high; P=low),
    findall(A-P-B, member(B, Bs), Cmds),
    append(T, Cmds, Stack),
    ht_put(Map, A, flp(Bs, S)),
    append(N0, Cmds, N1),
    !, simulate(Map, Inv, Stack, N1, N).
simulate(Map, Inv, [B0-P0-A|T], N0, N) :-
    ht_get( Map, A, con(Bs, In0) ),
    once((select(B0-_, In0, In1); In1=In0)),
    In = [B0-P0|In1],
    ht_get( Inv, A, Exp),
    once( (\+ (member(E, Exp), \+ memberchk(E-_, In)),
	   \+ memberchk(_-low, In),
	   P=low; P=high)),
    findall(A-P-B, member(B, Bs), Cmds),
    append(T, Cmds, Stack),
    ht_put(Map, A, con(Bs, In)),
    append(N0, Cmds, N1),
    !, simulate(Map, Inv, Stack, N1, N).
simulate(Map, Inv, [_|T], N0, N) :- simulate(Map, Inv, T, N0, N).

repeat(_, _, 0, N, N) :- !.
repeat(Map, Inv, M0, L0-H0, N) :-
    
    simulate(Map, Inv, [`0`-low-`0`], [`0`-low-`0`], Ps),


    % A trace to output button presses between "high" signals
    % at qn (the conjunction before rx).
    findall(_,
	    ( member(Code-high-`qn`, Ps),
	      string_codes(S, Code),
	      writeln([M0, S]) ), _),
        
    aggregate_all(count, member(_-low-_,Ps), L),
    aggregate_all(count, member(_-high-_,Ps), H),
    L1 is L0+L, H1 is H0+H, M is M0-1,
    !, repeat(Map, Inv, M, L1-H1, N).
repeat(Map, Inv, M, N) :-
    repeat(Map, Inv, M, 0-0, L-H), N is L*H.

lcm_(X,Y,Z) :- Z0 is gcd(X,Y), Z #= X*Y // Z0.
lcm([H|T],Z) :- foldl(lcm_,T,H,Z).


% Solution
% --------------------------------------------------------------------------
solve(File, Part1) :-
    ht_new(Map), ht_new(Inv),
    phrase_from_file(lines(Map, Inv), File),
    repeat(Map, Inv, 1000, Part1). % change to 100000 to use the trace.

% jx = 3907
% qz = 3911
% tt = 3931
% cq = 4021

