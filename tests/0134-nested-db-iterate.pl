main :-
	forall(between(1, 10, I), recordz(n, I)),
	forall(recorded(n, N), foo(N)).

foo(5) :-
	recorded(n, N, REF),
	N >= 5, N < 8,
	erase(REF),
	fail.
foo(5).
foo(N) :- writef("%d\n", [N]).
