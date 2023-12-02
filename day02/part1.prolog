:- set_prolog_flag(double_quotes, chars).

% Sequence of digits.
digits([]) --> [].
digits([X|Xs])  --> [X],{char_type(X,digit)},digits(Xs).

% Digits to number.
number(Ds,N) --> digits(Ds),{number_string(N,Ds)}.

% Entry within a record.
entry(blue,N) --> " ",digits(Ds),{number_string(N,Ds)}," blue".
entry(red,N) --> " ",digits(Ds),{number_string(N,Ds)}," red".
entry(green,N) --> " ",digits(Ds),{number_string(N,Ds)}," green".

% Parse a game record.
record([N-C|Rs]) --> entry(C,N),(";";","),!,record(Rs).
record([N-C]) --> entry(C,N).

% A game record.
game(N,L) --> "Game ",digits(Ds),{number_string(N,Ds)},":",record(L).

% Split a string into games.
lines([]) --> [].
lines([N-L|Ls]) --> game(N,L),"\n",lines(Ls).
lines([N-L]) --> game(N,L).

solve(File,Answer) :-
    open(File,read,Stream),
    read_string(Stream,_,String),
    string_chars(String,Chars),
    close(Stream),
    phrase(lines(Games),Chars),
    findall(
	G,
	(member(G-Rs,Games),
	 \+ (member(Gr-green,Rs),Gr>13),
	 \+ (member(Re-red,Rs),Re>12),
	 \+ (member(Bl-blue,Rs),Bl>14)),
	Valid),
    sumlist(Valid,Answer).
