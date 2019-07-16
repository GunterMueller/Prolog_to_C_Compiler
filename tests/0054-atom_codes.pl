main :-
	atom_codes(foo, "foo"),
	atom_codes([], "[]"),
	atom_codes(X, "foo"), X = foo.
