:- use_module(library(dcg/basics)).
:- use_module(library(chr)).

:- set_prolog_flag(chr_toplevel_show_store, false).

grid(_,_,[]) --> [].
grid(X0,_,Xs) --> "\n",{X is X0+1},!,grid(X,1,Xs).
grid(X0,Y0,[p(X0-Y0,V)|Xs]) --> [V],{Y is Y0+1},!,grid(X0,Y,Xs).
grid(X0,Y0,Xs) --> `.`,{Y is Y0+1},!,grid(X0,Y,Xs).

:- chr_constraint e/2.
:- chr_constraint clear/0.

clear \ e(_,_) <=> true.
clear <=> true.

e(X-Y,I-J) \ e(X-Y,I-J)  <=> true.

% Empty .|-
e(X-Y,I-J) ==> A is X+I, B is Y+J, p(A-B,46) | e(A-B,I-J).
e(X-Y,0-J) ==> A is X, B is Y+J, p(A-B,45) | e(A-B,0-J).
e(X-Y,I-0) ==> A is X+I, B is Y, p(A-B,124) | e(A-B,I-0).
% Splitter |
e(X-Y,0-J) ==> A is X, B is Y+J, p(A-B,124) | e(A-B,1-0), e(A-B,-1-0).
% Splitter -
e(X-Y,I-0) ==> A is X+I, B is Y, p(A-B,45) | e(A-B,0-1), e(A-B,-0-(-1)).
% Mirrors. /
e(X-Y,0-J) ==> A is X, B is Y+J, p(A-B,47) | K is -J, e(A-B,K-0).
e(X-Y,I-0) ==> A is X+I, B is Y, p(A-B,47) | K is -I, e(A-B,0-K).
% Mirrors. \
e(X-Y,I-J) ==> A is X+I, B is Y+J, p(A-B,92) | e(A-B,J-I).

eval(Energy) :-
    aggregate_all(count,X-Y,(find_chr_constraint(e(X-Y,_)),Y\=0,X\=0),Energy).

solve(File, Part1, Part2) :-
    phrase_from_file(grid(1,1,Ps),File),
    retractall(p(_,_)), maplist(asserta, Ps),
    call(e(1-0,0-1)), eval(Part1),
    aggregate_all(max(E), (call(clear), between(1,100,X),
			   member(A,[1-0,0-1,(-1)-0,0-(-1)]),
			   (call(e(0-X,A)); call(e(X-0,A))),
			   eval(E)), Part2).
