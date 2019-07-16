main :-
	tell(iotest),
	telling(S1),
	tell(user),
	display('zero\n'),
	tell(S1),
	tell("iotest2"),
	display('iotest2'), nl,
	told,
	display(one), nl,
	tell(S1),
	display('iotest 1'), nl,
	told,
	display(two), nl,
	see(iotest),
	seeing(S2),
	see(S2),
	peek_byte(C), display(C), nl,
	skip(116),		% t
	display('skip\n'),
	g0(101), g0(115), peek_byte(116), g0(116),
	display('peek\n'),
	g(49), g(10), peek_byte(-1), g(-1),
	nl,
	seen,
	tab(10), display('three\n').

g(X) :-	get(C), display(C), nl, X = C.
g0(X) :- get0(C), display(C), nl, X = C.
