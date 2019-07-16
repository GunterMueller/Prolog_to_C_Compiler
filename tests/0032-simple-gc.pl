main :-
	iota(1000, X),
	garbage_collect,
	dump(X).

iota(N, L) :- iota(0, N, L).
iota(N, N, []) :- !.
iota(N, M, [N|R]) :-
	!, N2 is N + 1, iota(N2, M, R).

dump([]) :- nl.
dump([X|Y]) :-
	!, display(X), display(' '), dump(Y).
