%1.
%  a. Remove all occurences of a certain atom from a list.
%  b. Out of a list of atoms produce a list of pairs (atom n), where the
%    atom appears in the original lista for n times. E.g.:
%    number([A B A B A C A], X) produces X = [[A 4] [B 2] [C 1]].

%a)
remove(_,[],[]).
remove(E,[E|T],R):-
	remove(E,T,R),
	!.
remove(E,[H|T],[Hr|Tr]):-
	Hr is H,
	remove(E, T, Tr).

%b)

isIn(E,[E|_]):-!.
isIn(E,[_|L]):-isIn(E,L).

appears(_,[],S,Cs):-
	S is Cs.

appears(E,[E|T],S, Cs):-
	Cs2 is Cs+1,
	appears(E, T, S, Cs2),
	!.

appears(E, [_|T], S, Cs2):-
	appears(E, T, S, Cs2).

exists_all(_,[]):-
	false,
	!.

exists_all(E,[H|_]):-
	isIn(E, H),
	true,
	!.

exists_all(E,[H|T]):-
	not(isIn(E,H)),
	exists_all(E,T).

start([],_).
start([H|T],Tr):-
	exists_all(H,Tr),
	start(T,Tr).

start([H|T], [Hr|Tr]):-
	not(exists_all(H,[Hr|Tr])),
	appears(H, [H|T], S, 0),
	Hr = [H, S],
	start(T, Tr).
