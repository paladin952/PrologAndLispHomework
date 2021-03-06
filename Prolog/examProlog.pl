occurences([], _,F, N):-
	N is F.


occurences([H|T], E, F, N):-
	H = E,
	NewF is F + 1,
	occurences(T, E,NewF, N).

occurences([H|T], E, F, N):-
	H \= E,
	occurences(T, E, F, N).


createList(E1, E2, [H1, H2]):-
	H1 is E1,
	H2 is E2.

isBefore([H|_], E, C, P, _):-
	H = E,
	C < P,
	!,
	true.


isBefore([_|_], _, C, P, 0):-
	C = P,
	!,
	false.

isBefore([H|T], E, C, P, 0):-
	C \= P,
	H \= E,
	NewC is C+1,
	isBefore(T, E, NewC, P, 0).

main([],_,_, []).

main([H|T],Co,Pos, [Hr|Tr]):-
	F is 0,
	NewPos is Pos +1,
	occurences(Co, H, F, R),
	not(isBefore(Co, H, 1, Pos, 0)),
	createList(H, R, Hr),
	main(T, Co, NewPos, Tr).
main([H|T],Co,Pos, Tr):-
	NewPos is Pos +1,
	isBefore(Co, H, 1, Pos, 0),
	main(T, Co, NewPos, Tr).


start(L, R):-
	Pos is 0,
	Co = L,
	main(L, Co,Pos, R).

