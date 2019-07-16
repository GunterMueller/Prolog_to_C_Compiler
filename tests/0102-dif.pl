main :-
	X = foo(1, A),
	Y = foo(1, B),
	display(dif), nl,
	dif(X, Y),
	display(work), nl,
	work(A),
	try(B).

work(1) :- display('one\n').

try(1) :-
	display('never\n').
try(2) :-
	display('ok\n').
