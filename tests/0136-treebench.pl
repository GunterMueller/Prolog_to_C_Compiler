%%% benchmark 23-trees vs rbtrees


:- ensure_loaded(library(tree23)).
:- ensure_loaded(library(rbtrees)).


count(10000).


/*
main :-
	writef("tree23: adding ...%f"),
	TT is clock,
	t1(T1),
	report(TT),
	writef("rbtree: adding ...%f"),
	TL is clock,
	t2(T2),
	report(TL),
	writef("tree23: lookup ...%f"),
	RT is clock,
	l1(T1),
	report(RT),
	writef("rbtree: lookup ...%f"),
	RL is clock,
	l2(T2),
	report(RL),
	nl.
*/

main :-
	t1(T1),
	t2(T2),
	l1(T1),
	l2(T2).

report(T) :-
	TN is round((clock - T) * 1000) / 1000,
	writef(" %d seconds\n", [TN]).

t1(T) :-
	empty_23(T0),
	add_t1(0, T0, T).

add_t1(N, T, T) :-
	count(N),
	!.
add_t1(I, T1, T) :-
	R is random(100000),
	put_23(I, T1, R, T2),
	atom_number(IA, I),
	put_23(IA, T2, R, T3),
	I2 is I + 1,
	!,
	add_t1(I2, T3, T).

l1(T) :-
	count(N),
	between(1, N, I),
	get_23(I, T, X),
	atom_number(IA, I),
	get_23(IA, T, Y),
	X =\= Y,
	writef("wrong result (tree23) at key %d: %d <> %d\n", [I, X, Y]),
	halt(1).
l1(_).

t2(T) :-
	rb_new(T0),
	add_t2(0, T0, T).

add_t2(N, T, T) :-
	count(N),
	!.
add_t2(I, T1, T) :-
	R is random(100000),
	rb_insert(T1, I, R, T2),
	atom_number(IA, I),
	rb_insert(T2, IA, R, T3),
	I2 is I + 1,
	!,
	add_t2(I2, T3, T).

l2(T) :-
	count(N),
	between(1, N, I),
	rb_lookup(I, X, T),
	atom_number(IA, I),
	rb_lookup(IA, Y, T),
	X =\= Y,
	writef("wrong result (rbtree) at key %d: %d <> %d\n", [I, X, Y]),
	halt(1).
l2(_).
