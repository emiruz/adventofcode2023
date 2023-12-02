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
	Power,
	(member(_-Rs,Games),
	 once((member(Re-red,Rs), \+ (member(Re2-red,Rs), Re2>Re))),
	 once((member(Gr-green,Rs), \+ (member(Gr2-green,Rs), Gr2>Gr))),
	 once((member(Bl-blue,Rs), \+ (member(Bl2-blue,Rs), Bl2>Bl))),
	 Power is Re*Gr*Bl),
	Valid),
    sumlist(Valid,Answer).
