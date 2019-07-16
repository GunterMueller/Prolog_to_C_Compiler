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
	findall(N/B, setof(A, legs(A, N), B), L1),
	write(L1), nl,
	findall(S, setof(N-L, bagof(A, legs(A, N), L), S), L2),
	write(L2), nl,
	findall(S, setof(X, (X = 2; X = 2), S), L3),
	write(L3), nl,
	findall(S, setof(X, (X = Y; X = Z), S), L4),
	write(L4), nl,
	findall(S, setof(X, member(X,[V,U,f(U),f(V)]), S), L5),
	write(L5), nl.
