main :-
	getenv("SHELL", X), display(X), nl,
	\+getenv('WHATEVER', _).
