main :-
	f(R) = W, acyclic_term(W),
	Q = f(Q), \+acyclic_term(Q),
	X = f(Y), acyclic_term(X),
	X = Q, \+acyclic_term(X), \+acyclic_term(Q),
	foo(A, f(A)), \+acyclic_term(A).

foo(X, X).
