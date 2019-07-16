main :- loop(10).

loop(0).
loop(X) :-
	display(X), nl,
	Y is X - 1, !, loop(Y).
