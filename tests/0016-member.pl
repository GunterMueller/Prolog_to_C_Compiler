main :-
	member(X, [1,2,3]), display(X), nl, fail.
main :-
	member(2, [1,2,3]), display('yes'), nl,
	(member(3, [1,2]); display('no'), nl),
	member(1, [Y|_]), display(Y), nl.
