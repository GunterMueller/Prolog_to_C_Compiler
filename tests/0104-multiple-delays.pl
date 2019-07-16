main :-
	freeze(X, (display('X fail\n'), fail)),
	freeze(X, display('X true\n')),
	display('two on X, one fails:\n'),
	foo(X),
	freeze(Y1, (display('Y1 fail\n'), fail)),
	freeze(Z1, display('Z1\n')),
	display('Y1 and Z1, one fails:\n'),
	bar(Y1, Z1),
	freeze(Y2, display('Y2\n')),
	freeze(Z2, display('Z2\n')),
	display('Y2 and Z2:\n'),
	bar(Y2, Z2),
	freeze(Y3, display('Y3\n')),
	freeze(Z3, display('Z3\n')),
	display('Y3 and Z3, joined:\n'),
	Y3 = Z3,
	display(first), nl,
	Z3 = 99,
	display(second), nl.

foo(1) :- display('foo\n').
foo(_).

bar(1, 2) :- display(bar), nl.
bar(_, _).

test.
