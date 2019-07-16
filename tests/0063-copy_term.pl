main :-
	copy_term(f(X, Y), Z), display(Z), nl,
	copy_term(X, -10), display(X), nl,
	copy_term(f(a, X1), f(X1, b)), display(X1), nl,
	copy_term(f(X2, X2), f(A, B)), display([X2, A, B]), nl,
	\+copy_term(a, 'ok'),
	X3 = f(Z1, Z2), copy_term(X3, U), U \== f(Z1, Z2), display([X3, U]), nl,
	\+((copy_term(f(a, X4), f(X4, b)), copy_term(f(a, X4), f(X4, b)))).
