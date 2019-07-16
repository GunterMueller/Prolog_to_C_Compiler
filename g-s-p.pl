%%% generate include-files for interp.pl from list of system-predicates


:- initialization main.

main :-
	op(200, fy, ['#', '?']),
	tell('pi_system_predicate.pl'),
	telling(SP),
	tell('pi_call_primitive.pl'),
	telling(CP),
	tell('pi_evaluate_op.pl'),
	telling(EO),
	run(SP, CP, EO),
	tell(SP), told,
	tell(CP), told,
	tell(EO), told,
	halt.

run(SP, CP, EO) :-
	repeat,
	read(TERM),
	gen_sys_pred(TERM, SP),
	gen_call_prim(TERM, CP),
	gen_eval_op(TERM, EO),
	TERM == end_of_file.

gen_sys_pred(DEF, SP) :-
	system_predicate_head(DEF, PRED),
	!,
	functor(PRED, NAME, ARITY),
	tell(SP),
	writeq(pi_system_predicate(NAME, ARITY)), put(46), nl.
gen_sys_pred(_, _).

gen_call_prim(DEF, CP) :-
	system_predicate_head(DEF, PRED),
	!,
	functor(PRED, NAME, ARITY),
	PRED =.. [_|PARGS],
	build_lists(1, PARGS, TERM, ARGS, CALLARGS),
	CALL =.. [NAME|CALLARGS],
	tell(CP),		    
	writeq((pi_call_primitive(NAME, ARITY, TERM) :- !, ARGS, CALL)),
	display('.\n').	
gen_call_prim(_, _).

gen_eval_op(arithmetic_operation(OP), EO) :-
	!,
	functor(OP, NAME, ARITY),
	OP =.. [_|PARGS],
	build_lists(1, PARGS, TERM, ARGS, CALLARGS),
	EXPR =.. [NAME|CALLARGS],
	tell(EO),
	writeq((pi_evaluate_op(NAME, ARITY, TERM, R) :- !, ARGS, R is EXPR)),
	display('.\n').
gen_eval_op(_, _).

build_lists(I, [], _, true, []).
build_lists(I, ['#'(_)|R] , T, (arg(I, T, V), pi_evaluate(V, X), VARS), [X|ARGS]) :-
	!,
	I2 is I + 1,
	build_lists(I2, R, T, VARS, ARGS).
build_lists(I, [_|R] , T, (arg(I, T, X), VARS), [X|ARGS]) :-
	I2 is I + 1,
	build_lists(I2, R, T, VARS, ARGS).

system_predicate_head(system_predicate(PRED), PRED).
system_predicate_head(system_predicate(PRED, _), PRED).

