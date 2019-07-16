:- include('tmp/0097-pb-simple.pl').

main :-
	errno(E), display(E),nl,
	getcwd(0, 256, P), display(P), nl,
	modf(12.34, X, Y), display(X/Y), nl.

