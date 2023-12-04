:- set_prolog_flag(double_quotes, chars).

% Spaces.
space([]) --> [' '].
space([_|Xs]) --> [' '],!,space(Xs).

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

intersect(As,Bs,Xs) :-
    findall(X,(member(X,As),member(X,Bs)),Xs),
    Xs\=[].

:- table copied/3.

copied(Id0-Ns0-Ps0,Cards,Total0) :-
    intersect(Ns0,Ps0,Xs),length(Xs,Len),
    findall(Total,(member(Id-Ns-Ps,Cards),
		   Id0+Len>=Id,Id>Id0,
		   copied(Id-Ns-Ps,Cards,Total)),Ts),
    sumlist(Ts,Sum),Total0 is Sum+1,!.
copied(_,_,1).

file_to_cards(File,Cards) :-
    open(File,read,Stream),
    read_string(Stream,_,String),
    string_chars(String,Chars),
    phrase(lines(Cards),Chars).

solve(File,part1,Answer) :-
    file_to_cards(File,Cards),
    findall(X,(member(_-Ns-Ps,Cards),intersect(Ns,Ps,Xs),
	       length(Xs,N),X is 2^(N-1)),Points),
    sumlist(Points,Answer).
solve(File,part2,Answer) :-
    file_to_cards(File,Cards),
    findall(N,(member(C,Cards),copied(C,Cards,N)),Points),
    sumlist(Points,Answer).
