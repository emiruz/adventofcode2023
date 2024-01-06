:- use_module(library(dcg/basics)).

solve(File, Part1, Part2) :-
    phrase_from_file(file(Time,Dist),File),
    pairs_keys_values(Pairs,Time,Dist),
    foldl([A,B,C]>>(sols(A,N),C is N*B),Pairs,1,Part1),
    concat(Time,T), concat(Dist,D),
    sols(T-D,Part2).

line([]) --> ("";"\n").
line([X|Xs]) --> blanks, number(X), line(Xs).
file(Time,Dist) --> "Time:", line(Time), "Distance:", line(Dist).

sols(T-D,N) :-
    Q is sqrt(T^2-4*D),
    Min is (T-Q)/2, Max is (T+Q)/2,
    (floor(Min)=:=Min->Xmn is Min+1; Xmn is ceiling(Min)),
    (floor(Max)=:=Max->Xmx is Max-1; Xmx is floor(Max)),
    N is floor(Xmx-Xmn+1).

concat(Ns,R) :-
    maplist(number_chars,Ns,Cs),
    flatten(Cs,S),number_chars(R,S).
