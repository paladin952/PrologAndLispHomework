%P1 10.
%  a. Determine the least common multiple of all elements of a linear
%  numerical list.
% b. Insert a given value v after the 1-st, 2-nd, 4-th, 8-th ... element
% of a list.

%
%Prints the content of a list
%
 printlist([]).

    printlist([X|List]) :-
        write(X), nl,
        printlist(List).


%a)
%calculates the greatest common divizer of 2 numbers
%
gcd(X, Y, G):-
	X = Y,
	G = X.

gcd(X, Y, G):-
	X<Y,
	Y1 is Y-X,
	gcd(X,Y1,G).
gcd(X,Y,G):-
	X>Y,
	gcd(Y,X,G).


%
% Calculates the least common multiple of 2 numbers and stores it in the
% last one
%
lcm(X, Y, Z):-
	gcd(X,Y,S),
	Z is abs(X * Y) / S.



start2([], Res):-
	write(Res).

start2([H1|T], Res):-
	lcm(H1, Res, Y),
	start2(T, Y).


%
%
%Starts the iteration in list
%S = lcm(l1, S)
%
%
start([H1|T]):-
	S is 1,
	lcm(H1, S, Y),
	start2(T,  Y).

%b)
go([], _, [], _, _).

go([H|T], E, [Hr1, Hr2|Tr], Cp, Sp):-
	Cp = Sp,
	Hr1 is H,
	Hr2 is E,
	NewSp is Sp * 2,
	NewCp is Cp + 1,
	go(T, E, Tr, NewCp, NewSp).

go([H|T], E, [Hr|Tr], Cp, Sp):-
	Cp \= Sp,
	Hr is H,
	NewCp is Cp + 1,
	go(T, E, Tr, NewCp, Sp).

mainGo(T, E, R):-
	Cp is 1,
	Sp is 1,
	go(T, E, R, Cp, Sp).





