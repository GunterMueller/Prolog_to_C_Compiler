main :-
	arg(1, foo(a), a),
	\+arg(2, foo(a), a),
	\+arg(2, foo(a, b), a),
	arg(1, [a,b], a),
	arg(2, [a,b], [b]).
	