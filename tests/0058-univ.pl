main :-
	[1,2] =.. ['.', 1, [2]],
	123 =.. [123],
	foo =.. [foo],
	X =.. [bar,x,Y], display(X), nl, X = bar(x, _),
	this(that, Z) =.. [NAME|ARGS],
	display(NAME), put(32), display(ARGS), nl,
	ARGS = [that, _], NAME = this, length(ARGS, 2).
