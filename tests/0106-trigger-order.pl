main :-
	test1,
	test2.
main :-
	test2.

test1 :-
	display(test1), nl,
	dif(X, 0),
	freeze(X, (writef('test1: %t\n', X), R is 100/X)),
	X = 0.

test2 :-
	display(test2), nl,
	freeze(X, (writef('test2: %t\n', X), R is 100/X)),
	dif(X, 0),
	X = 0.
