% Spaces.
space([]) --> " ".
space([_|Xs]) --> " ",space(Xs).

% Sequence of digits.
digits([]) --> [].
digits([X|Xs])  --> [X],{char_type(X,digit)},digits(Xs).

% Parse card number entries.
record([]) --> [].
record([N|Rs]) --> space(_),digits(Ds),{number_string(N,Ds)},record(Rs).

% A card record.
card(Id,Ns,Ps) --> "Card",space(_),digits(Ds),{number_string(Id,Ds)},":",
		   record(Ns)," |",record(Ps).

% Split a string into games.
lines([]) --> [].
lines([Id-Ns-Ps|Ls]) --> card(Id,Ns,Ps),"\n",lines(Ls).
lines([Id-Ns-Ps]) --> card(Id,Ns,Ps).

% Calculate number of common items.
intersect(As,Bs,N) :-
    findall(X,(member(X,As),member(X,Bs)),Xs),
    Xs\=[],length(Xs,N).

:- table copied/3.

% Calculate cumulative number of cards received.
copied(Id0-Ns0-Ps0,Cards,Total0) :-
    intersect(Ns0,Ps0,Len),
    findall(Total,(member(Id-Ns-Ps,Cards),
		   Id0+Len>=Id,Id>Id0,
		   copied(Id-Ns-Ps,Cards,Total)),Ts),
    sumlist(Ts,Sum),Total0 is Sum+1,!.
copied(_,_,1).

% Solutions to parts 1 & 2.
solve(File,part1,Answer) :-
    phrase_from_file(lines(Cards),File),
    findall(2^(N-1),(member(_-Ns-Ps,Cards),intersect(Ns,Ps,N)),Points),
    sumlist(Points,Answer).
solve(File,part2,Answer) :-
    phrase_from_file(lines(Cards),File),
    findall(N,(member(C,Cards),copied(C,Cards,N)),Points),
    sumlist(Points,Answer).
