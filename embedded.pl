%%% simple test for embedding


main :-
	write('one\n'),
	suspend(some("term"), X),
	write(' -> '), write(X), nl, X =:= 123,
	write('two\n'),
	garbage_collect,
	suspend(2, Y),
	write(' -> '), write(Y), nl, Y == some("term"),
	halt.
