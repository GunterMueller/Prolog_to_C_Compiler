%	serialize (palin25)
%	from Warren's thesis

serialize(L,R) :-
	pairlists(L,R,A),
	arrange(A,T),
	numbered(T,1,N).

pairlists([X|L], [Y|R], [pair(X,Y)|A]) :- pairlists(L,R,A).
pairlists([], [], []).

arrange([X|L], tree(T1, X, T2)) :-
	split(L, X, L1, L2),
	arrange(L1, T1),
	arrange(L2, T2).
arrange([], void).

split([X|L], X, L1, L2) :- !, split(L, X, L1, L2).
split([X|L], Y, [X|L1], L2) :- before(X,Y), !, split(L,Y,L1,L2).
split([X|L], Y, L1, [X|L2]) :- before(Y,X), !, split(L,Y,L1,L2).
split([], _, [], []).

before(pair(X1,Y1), pair(X2,Y2)) :- X1 < X2.

numbered(tree(T1, pair(X,N1), T2), N0, N) :-
	numbered(T1, N0, N1),
	N2 is N1+1,
	numbered(T2,N2,N).
numbered(_,N,N).

main :-
	serialize("ABLE WAS I ERE I SAW ELBA",X),
	write(X),nl.
