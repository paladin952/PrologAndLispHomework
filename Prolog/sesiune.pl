%power
pow(_, 0, 1).
pow(B, P, R):-
	P > 0,
	P1 is P-1,
	pow(B, P1, R1),
	R is B*R1.

pow2(_, 0, 1).
pow2(B, P, R):-
	P > 0,
	1 is P mod 2,
        P1 is P div 2,
	pow2(B, P1, R1),
	R is R1*R1*B.
pow2(B, P, R):-
        P > 0,
	0 is P mod 2,
        P1 is P div 2,
	pow2(B, P1, R1),
	R is R1*R1.
%length
len([], 0).
len([_|T], R):-
	len(T,R1),
	R is R1+1.

%permutation
perm(List,[H|Perm]):-
	deletee(H,List,Rest),
	perm(Rest,Perm).
perm([],[]).

deletee(X,[X|T],T).
deletee(X,[H|T],[H|NT]):-
	deletee(X,T,NT).

%combinations of k elements
comb(_,0,[]).
comb([H|T],N,[H|R]):-
	N>0,
	N1 is N-1,
	comb(T,N1,R).
comb([_|T],N,R):-
	N>0,
	comb(T,N,R).

%all combinationscomball([],[]).
comball([H|T], [H|R]):-
	comball(T,R).
comball([_|T], R):-
	comball(T,R).

%variations of k elements
var(_,0,[]).
var(L, K, [H|R]):-
	K>0,
	K1 is K-1,
	deletee(H,L,Rest),
	var(Rest, K1, R).

%Variations of k elements with sum s
%varSum1(L, K, S, C, R)
%not working
sum([],0).
sum([H|T],R):-
	sum(T,R1),
	R is R1+H.

varSum1(L,K,S,R1):-
	findall(X, (comb(L,K,X), sum(X,S), X), R),
	allVar(R, R1).

allVar([],[]).
allVar([H|T], [H1|R]):-
	findall(Y,perm(H,Y), H1),
	allVar(T,R).

%varSum2(L,K,S,C,R)
varSum2(_,0,S,C,[]):-
	S=C.
varSum2(L,K,S,C,[H|R]):-
	K>0,
	K1 is K-1,
	deletee(H,L,Rest),
	C1 is C+H,
	varSum2(Rest,K1,S,C1,R).

varSum(L,K,S,R):-
	findall(X,varSum2(L,K,S,0,X),R).

%Permutation pb
mydelete(X,[X|T],T).
mydelete(X,[H|T],[H|R]):-
	mydelete(X,T,R).

permpb([],_,[]).
permpb(L,Last,[H|R]):-
	deletee(H,L,Rest),
	Tmp1 is Last-3,
	Tmp2 is Last+3,
	H >=  Tmp1,
	H =< Tmp2,
	permpb(Rest,H,R),
	!.

permpbAux(L,[H|R]):-
	deletee(H,L,Rest),
	permpb(Rest,H,R).

permpbMain(L,R):-
	findall(X, permpbAux(L,X),R).

%delete maximum from list

maxim([H],H).
maxim([H|T],R):-
	maximAux(T,H,R).

maximAux([],X,X).
maximAux([H|T],X,R):-
	H>=X,
	maximAux(T,H,R).
maximAux([H|T],X,R):-
	H<X,
	maximAux(T,X,R).

sterge([],_,[]).
sterge([M|T],M,R):-
	!,
	sterge(T,M,R).
sterge([H|T],M,[H|R]):-
	!,
	sterge(T,M,R).

stergeLista([],[]).
stergeLista([H|T],[H1|R]):-
	is_list(H),
	maxim(H,M),
	sterge(H,M,H1),
	stergeLista(T,R).
stergeLista([H|T],[H|R]):-
	not(is_list(H)),
	stergeLista(T,R).
