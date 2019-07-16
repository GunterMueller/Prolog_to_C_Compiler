main :-
	VARS = [X, Y, Z],
	different(VARS),
	test(VARS).

test(VARS) :-
	VARS = [X, Y, Z],
	solve(X, Y, Z),
	write(VARS), nl, fail.
test(_).

solve(1, 1, 2).
solve(1, 2, 2).
solve(1, 2, 3).
solve(3, 2, 1).
solve(0, 2, 0).
solve(5, 6, 7).

%% not pairwise different, just adjacent ones
different([X]).
different([X|[Y|R]]) :-
	dif(X, Y),
	different([Y|R]).
