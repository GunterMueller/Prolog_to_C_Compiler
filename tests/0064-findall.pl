main :-
	findall(X, member(X, [1,2,3]), L), display(L), nl,
	findall(X, (member(X, [1,2,3]), !), L2), display(L2), nl,
	isotests.

legs(A, 6) :- insect(A).
legs(A, 4) :- animal(A).
legs(A, 8) :- spider(A).

spider(_) :- fail.
animal(_) :- fail.

insect(bee).
insect(ant).

isotests :-
	findall(X, insect(X), [bee, ant]),
	findall(X1, (X1 = 1; X1 = 2), S), display(S), nl,
	findall(X2, (X2 = Y; Y = X2), S2), display(S2), nl,
	findall(X3, fail, []),
	findall(X4, legs(_, X4), S3), display(S3), nl, % ISO book says [6,4,8], but SWI says [6, 6] (as do we)
	\+findall(X5, insect(X5), [ant, bee]).
