%Generate all permutation of N (N - given) with the property that for
%   every 2<=i<=n exists an 1<=j<=i, so |v(i)-v(j)|=1
elemAtPos([H|_], C, P, R):-
	C = P,
	R is H,
	!.

elemAtPos([_|T], C, P, R):-
	C \= P,
	NewC is C+1,
	elemAtPos(T, NewC, P, R).

checkBefore(_, _, _,_, 1):-
	!,
	true.

checkBefore([_], C, _, P, 0):-
	C = P,
	!,
	false.

checkBefore([H|T], C, E, P, 0):-
	C \= P,
	S is E-1,
	H = S,
	checkBefore(T, C, E, P, 1).

checkBefore([H|T], C, E, P, 0):-
	C \= P,
	S is E+1,
	H = S,
	checkBefore(T, C, E, P, 1).

checkBefore([H|T], C, E, P, 0):-
	C \= P,
	H \= E+1,
	H \= E-1,
	NewC is C + 1,
	checkBefore(T, NewC, E, P, 0).


 lengthList([],0).
 lengthList([_|T],N):-
   lengthList(T,X),
   N  is  X+1.

checkWholeList(L, C, 1):-
	lengthList(L, Len),
	C > Len,
	!,
	true.

checkWholeList(_, _, 0):-
	!,
	false.

checkWholeList(L, C, 1):-
	C > 1,
	lengthList(L, Len),
	C =< Len,
	elemAtPos(L, 1, C, Elem),
	checkBefore(L, 1, Elem, C, 0),
	NewC is C+1,
	checkWholeList(L, NewC, 1).

checkWholeList(L, C, 1):-
	lengthList(L, Len),
	C =< Len,
	C > 1,
	elemAtPos(L, 1, C, Elem),
	not(checkBefore(L, 1, Elem, C, 0)),
	checkWholeList(L, C, 0).

checkWholeList(L, C, 1):-
	C = 1,
	NewC is C + 1,
	checkWholeList(L, NewC, 1).

appendlist([], X, X).
appendlist([T|H], X, [T|L]) :-
 appendlist(H, X, L).

permutation([], []).
permutation([X], [X]) :-!.
permutation([T|H], X) :-
	permutation(H, H1),
	appendlist(L1, L2, H1),
	appendlist(L1, [T], X1),
	appendlist(X1, L2, X).


start(L, X):-
	permutation(L, X),
	checkWholeList(X, 1, 1).
