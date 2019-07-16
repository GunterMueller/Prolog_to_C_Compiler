main :-
	atomic_list_concat([abc,'123.0',45,9.1,'___'], X), writeq(X), nl.
