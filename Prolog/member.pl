member1(E,[E|_]).

member1(E,[_|L]):-
	member1(E,L).

