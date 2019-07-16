% ISO-example to ";" operator

main :-
	((insect(X), fly(X)); (has_legs(X, 6), fly(X))), display(X), nl, fail.
main.

insect(bee).
insect(ant).

fly(bee).

has_legs(X, 6) :- insect(X).
