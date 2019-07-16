main :-
	A = f(A), B = f(B),
	\+acyclic_term(A),
	recordz(foo, A),
	recorded(foo, X), \+acyclic_term(X),
	A = X,
	copy_term(A, A2), \+acyclic_term(A2),
	duplicate_term(A, A3), \+acyclic_term(A3).
