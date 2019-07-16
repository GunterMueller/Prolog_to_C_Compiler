main :-
	atom_concat(abc, def, abcdef),
	atom_concat(123, def, '123def'),
	atom_concat(X, def, '123def'), X == 123,
	atom_concat(123, Y, '123def'), Y == def,
	atom_concat(12, 34, '1234').
