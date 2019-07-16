% The sieve of Eratosthenes, from Clocksin & Mellish (pri2)
%	finding the prime numbers up to 98.	

sieve :- primes(98, X), write(X), nl.

primes(Limit, Ps) :- integers(2, Limit, Is), sift(Is, Ps).

integers(Low, High, [Low | Rest]) :- 
	Low =< High, !,
	M is Low+1,
	integers(M, High, Rest).
integers(_,_,[]).

sift([],[]).
sift([I | Is], [I | Ps]) :- remove(I,Is,New), sift(New, Ps).

remove(P,[],[]).
remove(P,[I | Is], [I | Nis]) :- \+(0 is I rem P), !, remove(P,Is,Nis).
remove(P,[I | Is], Nis) :- 0 is I rem P, !, remove(P,Is,Nis).

:- initialization sieve.
