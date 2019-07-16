main :-
	atom_chars(abc, X), X = [a,b,c],
	atom_chars(Y, [a,b,c]), Y = abc,
	number_chars(123, X2), X2 = ['1','2','3'],
	number_chars(Y2, ['1','2','3']), Y2 = 123.
