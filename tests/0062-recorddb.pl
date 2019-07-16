main :-	test, fail.
main :-
	garbage_collect,
	insert_list,
	delete_list,
	garbage_collect, name(foo, _),
	delete_list.

test :-
	recorda(foo, 123, X),
	S = abc,
	recorded(foo, N, X), N == 123,
	recordz(foo, S),
	recorded(foo, 123),
	erase(X),
	recorded(foo, S2, Z),
	display(S2), nl,
	S2 == S,
	erase(Z),
	\+recorded(foo, 123),
	recorda(foo(1), [list,1]),
	recorda(foo(2), some(term), Y),
	recorda(foo(3), another),
	recorda(foo(3), another(A,B,A,B)),
	show('once:', foo(_)),
	erase(Y),
	show('twice:', foo(_)).

show(X, Y) :-
	display(X), nl,
	recorded(Y, Z), display(Z), nl,
	fail.
show(_, _).

insert_list :-
	member(X,[1,2,3,4,5,6,7,8,9,10]),
	recordz(numbers, X),
	fail.
insert_list.

delete_list :-
	recorded(numbers, X, R), display(X), nl, erase(R),
	fail.
delete_list.
