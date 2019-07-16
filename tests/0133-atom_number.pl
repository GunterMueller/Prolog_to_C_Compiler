main :-
	atom_number('123', 123),
	atom_number('0.123', 0.123),
	atom_number(X, 123), X == '123',
	atom_number(Y, 123.1), Y == '123.1',
	atom_number('99', Z), Z == 99.
