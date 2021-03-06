max1([],M, S):-
	S is M.
max1([H|T],M, S):-
	H > M,
	max1(T, H, S).

max1([H|T],M, S):-
	H < M,
	max1(T, M, S).

deletee(_, [], []).
deletee(Me, [H|T], [Hr|Tr]):-
	Me \= H,
	Hr is H,
	deletee(Me, T, Tr).

deletee(Me, [H|T], Tr):-
	Me =:= H,
	deletee(Me, T, Tr).
%
%start([],Rs, Rs).
%start([H|T],[Hr|Tr], R):-
%	is_list(H),
%	max1(H,-999,M),
%	deletee(M, H, S),
%	Hr is S,
%	start(T, Tr, R).

%start([H|T],[Hr|Tr], R):-
%	not(is_list(H)),
%	Hr is H,
%	start(T, Tr, R).
start([], R, R).

start([H|T],[Hr|Tr], R):-
	is_list(H),
	max1(H, -999, S),
	Hr = [H, S],
	start(T, Tr, R).

start([H|T],[Hr|Tr], R):-
	Hr is H,
	start(T, Tr, R).

main(L, R):-
	start(L, [], R).


