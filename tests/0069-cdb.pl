main :-
	asserta((foo :- bar)),
	asserta((foo :- baz)),
	assertz(hello(X)),
	assertz(hello(1)),
	asserta(hello(no, ok), REF),
	clause(H, B, REF), H == hello(no, ok), B == true,
	retract(hello(X)), display(X), nl,
	show(foo),
	nl, show(hello(_)),
	abolish([foo/0, hello/1]),
	nl, show(foo),
	nl, show(hello(_, _)),
	erase(REF),
	nl, show(hello(_, _)).

show(HEAD) :-
	clause(HEAD, BODY), write((HEAD :- BODY)), nl, fail.
show(_).
