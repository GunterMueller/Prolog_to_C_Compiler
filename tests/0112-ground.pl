main :-
	\+ground(X),
	X = 123, ground(f(X)),
	A = f(A),
	recorda(foo, A),
	recorded(foo, A2),
	ground(A),
	ground(A2).
