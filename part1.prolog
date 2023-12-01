:- set_prolog_flag(double_quotes, chars).

% Any sequence.
seq([]) --> [].
seq([X|Xs]) --> [X],seq(Xs).

% Non-digit sequence.
non_digit --> [].
non_digit --> [X],{\+ char_type(X,digit)},non_digit.

% A digit.
digit(X)  --> [X],{char_type(X,digit)}.

% First and last digit.
code(A,A) --> non_digit,digit(A),non_digit.
code(A,B) --> non_digit,digit(A),seq(_),digit(B),non_digit.

% Split a string into lines.
lines([]) --> [].
lines([A-B|Ls]) --> code(A,B),"\n",!,lines(Ls).
lines([A-B]) --> code(A,B).

solve(File,Answer) :-
    open(File,read,Stream),
    read_string(Stream,_,String),
    string_chars(String,Chars),
    close(Stream),
    phrase(lines(Ls),Chars),
    findall(X,(member(A-B,Ls),number_string(X,[A,B])),Xs),
    sumlist(Xs,Answer).
