:- set_prolog_flag(double_quotes, chars).

to_coord([],_,_,Coords,Coords) :- !.
to_coord(['\n'|T],Row0,_,Acc,Coords) :-
    Row is Row0+1,!,
    to_coord(T,Row,1,Acc,Coords).
to_coord(['.'|T],Row,Col0,Acc,Coords) :-
    Col is Col0+1,!,
    to_coord(T,Row,Col,Acc,Coords).
to_coord([H|T],Row,Col0,Acc,Coords) :-
    Col is Col0+1,!,
    to_coord(T,Row,Col,[Row-Col0-H|Acc],Coords).
to_coord(Chars,Coords) :-
    to_coord(Chars,1,1,[],Coords0),
    reverse(Coords0,Coords).

summarise(Group0,X-Y-Y0-N) :-
    reverse(Group0,Group),
    Group=[X-Y-_|_],
    last(Group,_-Y0-_),
    findall(V,member(_-_-V,Group),Vs),
    number_string(N,Vs).

to_group([],Curr,Acc,Groups) :-
    Curr\=[],
    summarise(Curr,Summary),
    Groups=[Summary|Acc],!.
to_group([],_,Groups,Groups) :- !.
to_group([X-Y-V|T],[],Acc,Groups) :-
    char_type(V,digit),!,
    to_group(T,[X-Y-V],Acc,Groups).
to_group([_|T],[],Acc,Groups) :-
    to_group(T,[],Acc,Groups),!.
to_group([X-Y-V|T],Curr,Acc,Groups) :-
    char_type(V,digit),
    Curr=[X0-Y0-V0|_],
    char_type(V0,digit),
    X0=:=X,Y0+1=:=Y,!,
    to_group(T,[X-Y-V|Curr],Acc,Groups).
to_group(Coords,Curr,Acc,Groups) :-
    summarise(Curr,Summary),
    to_group(Coords,[],[Summary|Acc],Groups).
to_group(Coords,Groups) :-
    to_group(Coords,[],[],Groups).

star_group([],_,_,Acc,Acc) :- !.
star_group([X-Y0-Y-V|T],X_,Y_,Acc,Group) :-
    Y_ >= Y0-1,Y+1 >= Y_,
    X_ >= X-1,X+1 >= X_,!,
    star_group(T,X_,Y_,[V|Acc],Group).
star_group([_|T],X_,Y_,Acc,Group) :-
    star_group(T,X_,Y_,Acc,Group).
star_group(Groups,X,Y,Group) :-
    star_group(Groups,X,Y,[],Group).

prod(A,B,X) :- X is A*B.

solve(File,Answer) :-
    open(File,read,Stream),
    read_string(Stream,_,String),
    string_chars(String,Chars),
    close(Stream),
    to_coord(Chars,Coords),
    to_group(Coords,Groups),
    findall(X-Y,member(X-Y-'*',Coords),Stars),
    findall(
	Prod,
	(member(X-Y,Stars),
	 star_group(Groups,X,Y,Group),
	 length(Group,2),
	 foldl(prod,Group,1,Prod)),
	Gears),
    sumlist(Gears,Answer).
