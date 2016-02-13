%Se da o lista eterogena formata din numere, simboluri sau liste de
% numere. Se cere sa se
%determine suma numerelor din lista eterogena. 

suma_lista([],0).
suma_lista([H|T],S):-
       suma_lista(T,S1),
       S is S1+H.

suma_eterogena([],S, R):-
	S is R.
suma_eterogena([H|T],S, R):-
	number(H),
	Sp = R + H,
	suma_eterogena(T, S, Sp),
	!.

suma_eterogena([H|T], S, R):-
	is_list(H),
	suma_lista(H, Sl),
	Sp is R + Sl,
	suma_eterogena(T, S, Sp),
	!.

suma_eterogena([_|T], S, R):-
	suma_eterogena(T, S, R),
	!.



