main :-
	functor(123, 123, 0),
	functor(abc, abc, 0),
	functor([1,2], '.', 2),
	functor(foo(bar, 1), foo, 2),
	functor([], [], 0),
	functor(X, foo, 8),
	functor(X, foo, 8),
	display(X), nl.
