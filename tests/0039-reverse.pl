iota(N, L) :- iota(0, N, L).
iota(N, N, []).
iota(N, M, [N|R]) :-
	N2 is N + 1, iota(N2, M, R).

main :-
	iota(100, L),
	display(L), nl,
	reverse(L, X),
	display(X), nl.
