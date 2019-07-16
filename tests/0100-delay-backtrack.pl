main :-
	foo(X),
	X = 1,
	done.

foo(X) :-
	delay(X, display('defrost\n')),
	fail.
foo(_).

done :- display('done\n').

