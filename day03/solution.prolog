summary(Grp0,X-Y-Y0-N) :-
    reverse(Grp0,Grp),
    Grp=[X-Y-_|_],
    last(Grp,_-Y0-_),
    findall(V,member(_-_-V,Grp),Vs),
    number_string(N,Vs).

to_coord([],_,_,[],Acc,Acc) :- !.
to_coord([],_,_,Cur,Acc,[Grp|Acc]) :- summary(Cur,Grp),!.
to_coord([H|T],X0,Y0,Cur,Acc0,Coords) :-
    memberchk(H,['\n','.']),
    (H='.' ->Y1 is Y0+1;Y1=Y0),
    (H='\n'->X is X0+1,Y=1;X=X0,Y=Y1),
    (Cur=[]->Acc=Acc0;summary(Cur,Grp),Acc=[Grp|Acc0]),!,
    to_coord(T,X,Y,[],Acc,Coords).
to_coord([H|T],X,Y0,Cur,Acc0,Coords) :-
    \+ char_type(H,digit),
    Y is Y0+1,
    (Cur\=[]->summary(Cur,Grp),Acc=[Grp|Acc0];Acc=Acc0),!,
    to_coord(T,X,Y,[],[X-Y0-Y0-H|Acc],Coords).
to_coord([H|T],X,Y0,Cur,Acc,Coords) :-
    Y is Y0+1,
    to_coord(T,X,Y,[X-Y0-H|Cur],Acc,Coords).

file_to_coords(File,Coords) :-
    open(File,read,Stream),
    read_string(Stream,_,String),
    string_chars(String,Chars),
    to_coord(Chars,1,1,[],[],Coords).    

star_group(Coords,X_,Y_,Grp) :-
    findall(V,(member(X-Y0-Y-V,Coords),
	       number(V),
	       Y_>=Y0-1,Y+1>=Y_,X_>=X-1,X+1>=X_),Grp).

prod(A,B,X) :- X is A*B.

solve(part1,File,Answer) :-
    file_to_coords(File,Coords),
    findall(
	V,(member(X-Y0-Y-V,Coords),number(V),
	   \+ \+ (member(X_-Y_-_-V_,Coords),
		  \+ number(V_),
		  Y_>=Y0-1,Y+1>=Y_,X_>=X-1,X+1>=X_)),
	Filtered),
    sumlist(Filtered,Answer).

solve(part2,File,Answer) :-
    file_to_coords(File,Coords),
    findall(Prod,(member(X-Y-_-'*',Coords),
		  star_group(Coords,X,Y,Grp),
		  length(Grp,2),
		  foldl(prod,Grp,1,Prod)),Prods),
    sumlist(Prods,Answer).
