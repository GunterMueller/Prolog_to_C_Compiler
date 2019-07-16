main :-
	nth(I, [a,b,c], X), write(X), nl, fail.
main :-
	nth(X, [a,b,c], Y), write(X/Y), nl, fail.
main.
