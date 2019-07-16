:- public foo, bar/1.

main :-
	recorded('$public', p(A, B, C)),
	display([A, B]), nl,
	'$call'(C, [123]),
	fail.
main.

foo(X) :- display(foo(X)), nl.
bar(X) :- display(bar(X)), nl.
