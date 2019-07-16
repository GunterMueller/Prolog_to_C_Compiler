main :-
	recordz(foo, [abc, abc, def(abc)]),
	recorded(foo, X), display(X), nl,
	X = [Y, Y, def(Y)].
