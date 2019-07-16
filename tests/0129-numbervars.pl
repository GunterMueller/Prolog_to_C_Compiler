main :-
	T = foo(X, Y, [X|Z], yes, no(Z)),
	numbervars(T, 0, _),
	writeq(T), nl.
