main :-
	select(3, [1,2,3], [1,2]),
	select(X, Y, [1,2]), display(X), nl, fail.
main :-
	select(X, [1,2,3], Y), display(X), put(32), display(Y), nl, fail.
main.

