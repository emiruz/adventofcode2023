:- use_module(library(dcg/basics)).

% Parser
% ----------------------------------------------------------------------
line1(C-L) --> [C], { memberchk(C,`LRUD`) }, " ", number(L) ,string(_), ")".
line2(Cmd) --> string(_), " (#", string(S), ")", { hex2cmd(S, Cmd) }.
lines(_,[]) --> [].
lines(F,[L|Ls]) --> call(F,L), "\n", !, lines(F, Ls).
lines(F,[L]) --> call(F,L).

hex2cmd(S, Cmd-Length) :-
    append(Prefix,[C],S),   
    append(`0x`, Prefix, Hex),
    number_codes(Length, Hex),
    ([C]=`0`, [Cmd]=`R`;
     [C]=`1`, [Cmd]=`D`;
     [C]=`2`, [Cmd]=`L`;
     [C]=`3`, [Cmd]=`U`).

% Geometry
% ----------------------------------------------------------------------
corners([], Acc, Acc) :- !.
corners([C-L|T], [I-J|T2], Result) :-
    ([C]=`R`, X=I, Y is J+L;
     [C]=`L`, X=I, Y is J-L;
     [C]=`U`, Y=J, X is I-L;
     [C]=`D`, Y=J, X is I+L),
    !, corners(T, [X-Y,I-J|T2], Result).
corners(Ins, Corners) :-
    corners(Ins,[1-1],Corners).

% Shape area using the determinant method.
area([X-Y,I-J], Acc, Result) :-
    Result is Acc + 0.5*(X*J - I*Y),!.
area([X-Y,I-J|T], Acc0, Result) :-
    Acc is Acc0 + 0.5*(X*J - I*Y),!,
    area([I-J|T], Acc, Result).

% Pick's theorem to convert area and boundary
% into total cover.
pick(Boundary, Area, Total) :- 
    I is Area - (Boundary / 2) + 1,
    Total is I + Boundary.

% Solution
% ----------------------------------------------------------------------
solve_(File, F, Answer) :-
    phrase_from_file(lines(F, Ins), File),
    aggregate_all(sum(L), member(_-L, Ins), Bounds),
    corners(Ins, Cs), area(Cs, 0, Area),
    pick(Bounds, Area, Answer).

solve(File, Part1, Part2) :-
    solve_(File, line1, Part1),
    solve_(File, line2, Part2).
