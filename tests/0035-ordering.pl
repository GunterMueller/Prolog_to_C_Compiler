main :-
	DATA = [123, 456, 1.23, 4.56, ABC, DEF, abc, def, ab, abc, [], [1, 2], [4, -1], foo(A, b),
		bar(B, b), bar(1, 2), bar(1), '.'(1,2), '[]'],
	member(X, DATA),
        member(Y, DATA),
	(X @> Y -> C1 = yes; C1 = no),
	(X @< Y -> C2 = yes; C2 = no),
	(X @>= Y -> C3 = yes; C3 = no),
	(X @=< Y -> C4 = yes; C4 = no),
	show(X, '@>', Y, C1),
	show(X, '@<', Y, C2),
	show(X, '@>=', Y, C3),
	show(X, '@=<', Y, C4),
	fail.
main.

show(X, CMP, Y, R) :- display(X), display(' '), display(CMP), display(' '), display(Y), display('\t -> '), display(R), nl.
