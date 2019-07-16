legs(A, 6) :- insect(A).
legs(A, 4) :- animal(A).
legs(A, 8) :- spider(A).

insect(bee).
insect(ant).
animal(horse).
animal(cat).
animal(dog).
spider(tarantula).

main :-
	findall(N/B, bagof(A, legs(A, N), B), L1),
	write(L1), nl,
	findall(S/Y, bagof(X, (X = Y; X = Z; Y = 1), S), L2),
	write(L2), nl.
