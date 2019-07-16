main :-
	freeze(X, (display('X='), display(X), nl)),
	foo(X),
	bar,
	X = 1,
	bar,
	done.

foo(Y).

done :- display('done\n').

bar :- display(bar), nl.

