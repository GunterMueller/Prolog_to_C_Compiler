:- meta_predicate xforall(0, 0), xmaplist(1, +).

main :-
	xforall(member(X, [1, 2, 3]), writef("%d\n", X)),
	xmaplist(writef("%d\n"), [a, b, c]),
	xmaplist(integer, [1,2,3]),
	\+xmaplist(integer, [1,2,a]).

	
xforall(X, Y) :-
	'$check_callable'(X, PTR1, VARS1),
	'$check_callable'(Y, PTR2, VARS2),
	'$call'(PTR1, VARS1),
	\+'$call'(PTR2, VARS2), !, fail.
xforall(_, _).

xmaplist(C, L) :-
	'$check_callable'(C, P, A),
	xmaplist(L, P, A).

xmaplist([], _, _) :- !.
xmaplist([X|R], P, A) :-
	append(A, [X], A2),	
	!, '$call'(P, A2), xmaplist(R, P, A).

'$check_callable'('$meta_call'(P, A), P, A) :- foreign_pointer(P), !.
'$check_callable'(X, _, _) :- throw(type_error(callable, X)).
