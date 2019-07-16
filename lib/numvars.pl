%%%% numbering variables


numbervars(X, S, E) :-
	var(X),
	!,
	X = '$VAR'(S),
	E is S + 1.
numbervars(X, S, S) :-
	atomic(X),
	!.
numbervars([X|Y], S, E) :-
	numbervars(X, S, E1),
	numbervars(Y, E1, E).
numbervars(X, S, E) :-
	X =.. [_|ARGS],
	numbervars(ARGS, S, E).
