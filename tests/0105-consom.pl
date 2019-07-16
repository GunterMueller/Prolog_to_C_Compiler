main :-
	freeze(V, consom(V)),
	product(0, V).

product(X, [X|L]) :-
	X1 is X + 1, X < 100, product(X1, L).

consom([X|L]) :-
	display(X), nl,
	freeze(L, consom(L)).
