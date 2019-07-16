main :-
	[1|2] = '.'(1,2),
	foo('.'(1,'.'(2,[]))),
	bar([1,2]).

foo([X, Y]) :- display([X, Y]), nl.
bar('.'(X, '.'(Y, Z))) :- display([X, Y|Z]), nl.
