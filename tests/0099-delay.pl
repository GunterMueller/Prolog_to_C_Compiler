main :-
	V = 99, delay(V, (display(now), nl)),
	delay(X, (display(fail), nl, fail)),
	Y = 1,
	X = 99,
	done.
main :-
	delay(Z, (display([Z]), nl)),
	next(Z).

done :- display(done), nl.
done :- display(bad), nl.

next(123) :-
	display(next), nl.
