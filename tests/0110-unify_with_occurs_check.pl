main :-
	unify_with_occurs_check(Q, R),
	\+unify_with_occurs_check(A, f(A)),
	X = f(X), Y = X, \+acyclic_term(X), \+acyclic_term(Y),
	X1 = f(X1), Y1 = X1, \+unify_with_occurs_check(X1, Y1),
	X2 = f(X2), Y2 = f(Y2), \+unify_with_occurs_check(X2, Y2).
