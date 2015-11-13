%13. Given a list of integer numbers and lists of integer numbers.
%       In every sub-list, write two times elements from N to N steps.

duplicateInList([], _, _, _, []). % stop if there is nothing left

duplicateInList([H| T],C, N, InitVal, [Hr1|Tr]):-
	C = N,
	Hr1 is H * 2,
	NewC is C + 1,
	NewN is N + InitVal,
	duplicateInList(T, NewC, NewN, InitVal, Tr).

duplicateInList([H | T], C, N, InitVal, [Hr | Tr]):-
	C \= N,
	NewC is C + 1,
	Hr is H,
	duplicateInList(T, NewC, N, InitVal, Tr).

iterate([], _, []).

iterate([H|T], InitVal, [Hr|Tr]):-
	is_list(H),
	duplicateInList(H, 1, InitVal, InitVal, Hr),
	iterate(T, InitVal, Tr).

iterate([H|T], InitVal, [Hr|Tr]):-
	not(is_list(H)),
	Hr is H,
	iterate(T, InitVal, Tr).

start(L, InitVal, R):-
	iterate(L, InitVal, R).


