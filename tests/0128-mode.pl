:- mode foo(+,-),
	bar(+).

main :-
	foo([x,y], Z), display(Z), nl,
	bar(123).

foo([X, Y], 1/Y).
bar(123).
