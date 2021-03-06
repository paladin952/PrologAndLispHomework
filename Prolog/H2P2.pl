%2. Determine the predecessor of a number represented as digits in a
% list. E.g.:
%    [1 9 3 6 0 0] --> [1 9 3 5 9 9]
%
%
%
%We reverse the list
reverse([],Z,Z).

reverse([H|T],Z,Acc) :-
	reverse(T,Z,[H|Acc]).

%we go throug each element and decrement it if > 0
%If less than 0 we set it to 9 and go to the next
% the last parameters tells us if we need to decrement the number%or
% just to add it to the result list
decrement([], [], 0).

decrement([H|T], [Hr|Tr], 0):-
	Hr is H,
	decrement(T, Tr, 0).

decrement([H|T], [Hr|Tr], 1):-
	H > 0,
	Hr is H-1,
	decrement(T, Tr, 0)
	;
	H < 1,
	Hr is 9,
	decrement(T, Tr, 1).

%reverse the list
%decrement
%reverse again
main(L, R):-
	reverse(L, Rev, []),
	decrement(Rev, S, 1),
	reverse(S, R, []).
