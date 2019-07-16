iota(N, L) :- iota(0, N, L).
iota(N, N, []).
iota(N, M, [N|R]) :-
	N2 is N + 1, iota(N2, M, R).

main :-
	union([1,2,3,4,5],[3,4,5,6,7],X),
	display(X), nl,
	intersection([1,2,3,4,5],[3,4,5,6,7],Y),
	display(Y), nl,
	subtract([1,2,3,4,5],[3,4,5,6,7],Z),
	display(Z), nl.
