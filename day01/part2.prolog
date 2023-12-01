:- set_prolog_flag(double_quotes, chars).

eos_([], []).

% Compound to simple words.
comp2simp,[] --> call(eos_).
comp2simp,"twoone" --> "twone",!,comp2simp.
comp2simp,"eighttwo" --> "eightwo",!,comp2simp.
comp2simp,"eightthree" --> "eighthree",!,comp2simp.
comp2simp,"oneeight" --> "oneight",!,comp2simp.
comp2simp,"threeeight" --> "threeight",!,comp2simp.
comp2simp,"fiveeight" --> "fiveight",!,comp2simp.
comp2simp,"nineeight" --> "nineight",!,comp2simp.
comp2simp,"sevennine" --> "sevenine",!,comp2simp.
comp2simp,[X] --> [X],!,comp2simp.

% Words to numbers.
word2num,[] --> call(eos_).
word2num,"1" --> "one",!,word2num.
word2num,"2" --> "two",!,word2num.
word2num,"3" --> "three",!,word2num.
word2num,"4" --> "four",!,word2num.
word2num,"5" --> "five",!,word2num.
word2num,"6" --> "six",!,word2num.
word2num,"7" --> "seven",!,word2num.
word2num,"8" --> "eight",!,word2num.
word2num,"9" --> "nine",!,word2num.
word2num,[X] --> [X],word2num.

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
lines([A-B|Ls]) --> comp2simp,word2num,code(A,B),"\n",!,lines(Ls).
lines([A-B]) --> comp2simp,word2num,code(A,B).

solve(File,Answer) :-
    open(File,read,Stream),
    read_string(Stream,_,String),
    string_chars(String,Chars),
    close(Stream),
    phrase(lines(Ls),Chars),
    findall(X,(member(A-B,Ls),number_string(X,[A,B])),Xs),
    sumlist(Xs,Answer).
