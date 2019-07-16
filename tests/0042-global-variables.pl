:- global_variable(foo), global_variable(bar).

main :- 
	global_ref(foo, X), display(X), nl,
	global_set(foo, foo(abc, def)),
	garbage_collect,
	global_ref(foo, Y), display(Y), nl.
