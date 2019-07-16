%%% "asembly" of pseudo-instructions into C


%% assembler entry point

assemble_file(FILE, STATE) :-
	tell(FILE),
	generate_header,
	assemble_literals(STATE, S1),
	default_setting(entry_point, MAIN),
	mangle_name(MAIN, MMAIN),
	gen('#define INIT_GOAL ', MMAIN, '$0\n'),
	assemble_global_variables(0, N),
	gen('ENTRY_POINT{\n'),
	gen('global_variable_counter=', N, ';\n'),
	gen('BOILERPLATE;{\n'),
	assemble_instructions(S1),
	gen('}STARTUP;}\n'),
	generate_trailer,
	told.

assemble_global_variables(I, N) :-
	recorded(global_variables, NAME, REF),
	erase(REF),
	gen('#define ', NAME, ' ', I, '\n'),
	I2 is I + 1,
	!,
	assemble_global_variables(I2, N).
assemble_global_variables(N, N).

assemble_instructions(STATE) :-
	recorded(code, OP, REF),
	erase(REF),
	assemble(OP, STATE, S2),
	!,
	assemble_instructions(S2).
assemble_instructions(_).

generate_header :-
	recorded(source_file, FILE),
	gen('// GENERATED BY pc FROM ', FILE, '\n'),
	(\+recorded(uses_delay, _); gen('#define USE_DELAY\n')),
	gen('#define COMPILED_PROLOG_PROGRAM\n#include "pc.h"\n'),
	generate_verbatim_code.

generate_verbatim_code :-
	recorded(verbatim_code, CODE),
	gen(CODE, '\n'),
	fail.
generate_verbatim_code.

generate_trailer :-
	gen('// END OF FILE\n').


%% assemble pseudo ops

assemble(enter(NAME, ARITY, LBL), S1, S) :-
	recorded(defined, NAME/ARITY),
	mangle_name(NAME, MNAME),
	gen_label(L, S1, S),
	gen('}\n\n#undef CURRENT_NAME\n#undef CURRENT_ARITY\n#define CURRENT_NAME "'),
	gen(NAME, '"\n#define CURRENT_ARITY ', ARITY, '\nDECLARE_PINFO("', NAME),
	gen('",', ARITY,',', L),
	gen(');\n#undef PREVIOUS_PINFO\n#define PREVIOUS_PINFO &', L),
	gen('\n', MNAME, '$', ARITY, ':\n'),
	gen('{ENTER(', LBL, ');\n'),
	(ARITY =:= 0; gen('A[0]=deref(A[0]);\n')). % for indexing
assemble(enter(_, _, _), S, S) :-
	%% drop everything until next 'enter'
	( forall(recorded(code, OP, REF), (OP \= enter(_, _, _), erase(REF)))
	; true).
assemble(environment(SIZE), S, S) :-
	gen('#undef CURRENT_ENVIRONMENT_SIZE\n#define CURRENT_ENVIRONMENT_SIZE ', SIZE, '\n'),
	(SIZE =:= 0; gen('ENVIRONMENT(', SIZE, ');\n')).
assemble(determinate_exit, S, S) :- gen('DETERMINATE_EXIT;\n').
assemble(exit(L), S, S) :- gen('EXIT(', L, ');\n').
assemble(redo, S, S) :- gen('REDO;\n').
assemble(set_redo(L), S, S) :- gen('SET_REDO(&&', L, ');\n').
assemble(no_redo, S, S) :- gen('SET_REDO(NULL);\n').
assemble(copy_choice_point(L), S, S) :- gen('COPY_CHOICE_POINT(&&', L, ');\n').
assemble(push_choice_point(L), S, S) :- gen('PUSH_CHOICE_POINT(&&', L, ');\n').
assemble(pop_choice_point, S, S) :- gen('POP_CHOICE_POINT;\n').
assemble(save_choice_points, S, S) :- gen('SAVE_CHOICE_POINTS;\n').
assemble(restore_choice_points, S, S) :- gen('RESTORE_CHOICE_POINTS;\n').
assemble(adjust_choice_point(L), S, S) :- gen('ADJUST_CHOICE_POINT(&&', L, ');\n').
assemble(cut(L), S, S) :- gen('CUT(', L, ');\n').
assemble(label(LABEL), S, S) :- gen('}', LABEL, ':{\n').
assemble(local(N, R), S, S) :- gen('X ', R, '=E[', N, '];\n').
assemble(unify(R1, R2), S, S) :- gen('if(!unify(', R1, ',', R2, ')) FAIL;\n').
assemble(not_unify(R1, R2), S, S) :- gen('if(unify(', R1, ',', R2, ')) FAIL;\n').
assemble(argument(INDEX, R), S, S) :- gen('X ', R, '=A[', INDEX, '];\n').
assemble(assign(N, R), S, S) :- gen('E[', N, ']=', R, ';\n').
assemble(make_variable(R), S, S) :- gen('X ', R, '=make_var();\n').
assemble(literal(N, R, _), S, S) :- gen('X ', R, '=literal_', N, ';\n').
assemble(push_catcher(L), S, S) :- gen('PUSH_CATCHER(&&', L, ');\n').
assemble(pop_catcher, S, S) :- gen('POP_CATCHER;\n').
assemble(enter_catcher, S, S) :- gen('SET_WHERE(PREVIOUS_PINFO);\n').
assemble(unify_throw(R), S, S) :- gen('if(!unify(catch_top->ball,', R, ')) RETHROW;\n').
assemble(unify_args(L, NS), S, S) :-
	gen('static X ', L, '[]={'),
	generate_literal_list(NS),
	gen('NULL};\nif(!unify_args(C0,A,', L, ')) FAIL;\n').
assemble(unify_facts(L, DATA, TABLE, TILEN, ITABLE, TALEN, ATABLE, TSLEN, STABLE), S1, S2) :-
	gen_label(L2, S1, S3),
	gen_label(L3, S3, S4),
	gen_label(L4, S4, S5),
	gen_label(LIT, S5, S6),
	gen_label(LAT, S6, S7),
	gen_label(LST, S7, S2),
	gen('static int ', L4, '[]={'),
	generate_data_list(TABLE),
	gen('};\nstatic X ', L, '[]={'),
	forall(member(XS, DATA), generate_literal_list(XS)),
	gen('NULL};\n'),
	( ITABLE == none
	-> LLIT = 'NULL'
	; gen('static BLOCK_INTEGER_DISPATCH ', LIT, '[]={'),
	  assemble_integer_dispatch(0, TILEN, ITABLE, 0),
	  gen('};\n'),
	  LLIT = LIT
	),
	( ATABLE == none
	-> LLAT = 'NULL'
	; gen('static BLOCK_SYMBOL_DISPATCH ', LAT, '[]={'),
	  assemble_atom_dispatch(0, TALEN, ATABLE, '0'),
	  gen('};\n'),
	  LLAT = LAT
	),
	( STABLE == none
	-> LLST = 'NULL'
	; gen('static BLOCK_STRUCTURE_DISPATCH ', LST, '[]={'),
	  assemble_structure_dispatch(0, TSLEN, STABLE, '0'),
	  gen('};\n'),
	  LLST = LST
	),
	gen_list(['UNIFY_BLOCK(', L, ',', L2, ',', L3, ',', L4, ',', TILEN,
		  ',', LLIT, ',', TALEN, ',', LLAT, ',', TSLEN, ',', LLST,
		  ');\n']).
assemble(make_term(RLIST, R), S, S) :-
	length(RLIST, N),
	N1 is N - 1,
	RLIST = [F|ARGS],
	gen('X ', R, '=STRUCTURE(', F, ','),
	gen(N1, ');\n'),
	generate_slot_inits(R, 1, ARGS).
assemble(make_pair(CAR, CDR, R), S, S) :-
	gen('X ', R, '=make_pair('),
	gen(CAR, ',', CDR, ');\n').
assemble(jump(LABEL), S, S) :- gen('goto ', LABEL, ';\n').
assemble(remove_choice_points, S, S) :- gen('CLEARCP;\n').
assemble(fail, S, S) :- gen('FAIL;\n').
assemble(eq(R1, R2), S, S) :- gen('if(deref(', R1, ')!=deref(', R2, ')) FAIL;\n').
assemble(identical(R1, R2), S, S) :- gen('if(!is_identical(', R1, ',', R2, ')) FAIL;\n').
assemble(not_identical(R1, R2), S, S) :- gen('if(is_identical(', R1, ',', R2, ')) FAIL;\n').
assemble(numerically_equal(R1, R2), S, S) :- gen('if(!is_num_eq(', R1, ',', R2, ')) FAIL;\n').
assemble(numerically_not_equal(R1, R2), S, S) :- gen('if(is_num_eq(', R1, ',', R2, ')) FAIL;\n').
assemble(numerically_greater(R1, R2), S, S) :- gen('if(!is_num_gt(', R1, ',', R2, ')) FAIL;\n').
assemble(numerically_less(R1, R2), S, S) :- gen('if(!is_num_lt(', R1, ',', R2, ')) FAIL;\n').
assemble(numerically_greater_or_equal(R1, R2), S, S) :- gen('if(is_num_lt(', R1, ',', R2, ')) FAIL;\n').
assemble(numerically_less_or_equal(R1, R2), S, S) :- gen('if(is_num_gt(', R1, ',', R2, ')) FAIL;\n').
assemble(call(NAME, RLIST, LABEL), S, S) :-
	length(RLIST, ARITY),
	(ARITY == 0; gen('A=arg_top;\n')),
	assemble_arguments(RLIST, 0),
	mangle_name(NAME, MNAME),
	gen('CALL(', MNAME, '$', ARITY, ',&&'),
	gen(LABEL, ');}\n', LABEL, ':{\nSET_WHERE(PREVIOUS_PINFO);\n').
assemble(final_call(NAME, RLIST, LABEL), S, S) :-
	length(RLIST, ARITY),
	gen('if(C==C0+1) POP_ARGUMENTS;\n'),
	(ARITY == 0; gen('A=arg_top;\n')),
	assemble_arguments(RLIST, 0),
	mangle_name(NAME, MNAME),
	gen('FINAL_CALL(', MNAME, '$', ARITY, ',&&'),
	gen(LABEL, ');}\n', LABEL, ':{\nSET_WHERE(PREVIOUS_PINFO);\n').
assemble(call_address(RADR, RARGS, LABEL), S, S) :-
	gen('A=arg_top;\npush_argument_list(', RARGS, ');\n'),
	gen('CALL(*((void*)slot_ref(deref(', RADR, '),0)),&&', LABEL, ');}\n'),
	gen(LABEL, ':{\n').
assemble(final_call_address(RADR, RARGS, LABEL), S, S) :-
	gen('if(C==C0+1) POP_ARGUMENTS;\n'),
	gen('A=arg_top;\npush_argument_list(', RARGS, ');\n'),
	gen('FINAL_CALL(*((void*)slot_ref(deref(', RADR, '),0)),&&', LABEL, ');}\n'),
	gen(LABEL, ':{\n').
assemble(tail_call(NAME, RLIST), S, S) :-
	length(RLIST, ARITY),
	gen('POP_ARGUMENTS;\n'),
	(ARITY == 0; gen('A=arg_top;\n')),
	assemble_arguments(RLIST, 0),
	mangle_name(NAME, MNAME),
	gen('TAIL_CALL(', MNAME, '$', ARITY, ');\n').
assemble(tail_call_address(RADR, RARGS), S, S) :-
	gen('POP_ARGUMENTS;\nA=arg_top;\npush_argument_list(', RARGS, ');\n'),
	gen('TAIL_CALL(*((void*)slot_ref(deref(', RADR, '),0)));\n').
assemble(foreign_call(NAME, 0), S, S) :- gen('if(!', NAME, '(C0)) FAIL;\n').
assemble(foreign_call(NAME, RLIST), S, S) :-
	gen('if(!', NAME, '(C0'),
	generate_foreign_arguments(RLIST),
	gen(')) FAIL;\n').	%XXX doesn't pop
assemble(predicate_address(N, A, R), S, S) :-
	mangle_name(N, MNAME),
	gen('X ', R, '=POINTER(&&', MNAME),
	gen('$', A, ');\n').
assemble(add(R1, R2, R3), S, S) :- gen('X ', R3, '=num_add(', R1, ','),	gen(R2, ');\n').
assemble(subtract(R1, R2, R3), S, S) :-	gen('X ', R3, '=num_sub(', R1, ','), gen(R2, ');\n').
assemble(multiply(R1, R2, R3), S, S) :-	gen('X ', R3, '=num_mul(', R1, ','), gen(R2, ');\n').
assemble(divide(R1, R2, R3), S, S) :- gen('X ', R3, '=num_div(', R1, ','), gen(R2, ');\n').
assemble(quotient(R1, R2, R3), S, S) :- gen('X ', R3, '=num_quo(', R1, ','), gen(R2, ');\n').
assemble(mod(R1, R2, R3), S, S) :- gen('X ', R3, '=num_mod(', R1, ','), gen(R2, ');\n').
assemble(rem(R1, R2, R3), S, S) :- gen('X ', R3, '=num_rem(', R1, ','), gen(R2, ');\n').
assemble(max(R1, R2, R3), S, S) :- gen('X ', R3, '=num_max(', R1, ','), gen(R2, ');\n').
assemble(min(R1, R2, R3), S, S) :- gen('X ', R3, '=num_min(', R1, ','), gen(R2, ');\n').
assemble(bitwise_and(R1, R2, R3), S, S) :- gen('X ', R3, '=num_and(', R1, ','), gen(R2, ');\n').
assemble(bitwise_or(R1, R2, R3), S, S) :- gen('X ', R3, '=num_or(', R1, ','), gen(R2, ');\n').
assemble(shift_left(R1, R2, R3), S, S) :- gen('X ', R3, '=num_shl(', R1, ','), gen(R2, ');\n').
assemble(shift_right(R1, R2, R3), S, S) :- gen('X ', R3, '=num_shr(', R1, ','), gen(R2, ');\n').
assemble(exponent(R1, R2, R3), S, S) :- gen('X ', R3, '=num_pow(', R1, ','), gen(R2, ');\n').
assemble(xor(R1, R2, R3), S, S) :- gen('X ', R3, '=num_xor(', R1, ','), gen(R2, ');\n').
assemble(bitwise_not(R1, R2), S, S) :- gen('X ', R2, '=num_not(', R1, ');\n').
assemble(abs(R1, R2), S, S) :- gen('X ', R2, '=num_abs(', R1, ');\n').
assemble(atan(R1, R2), S, S) :- gen('X ', R2, '=num_atan(', R1, ');\n').
assemble(ceiling(R1, R2), S, S) :- gen('X ', R2, '=num_ceiling(', R1, ');\n').
assemble(cos(R1, R2), S, S) :- gen('X ', R2, '=num_cos(', R1, ');\n').
assemble(exp(R1, R2), S, S) :- gen('X ', R2, '=num_exp(', R1, ');\n').
assemble(float(R1, R2), S, S) :- gen('X ', R2, '=num_float(', R1, ');\n').
assemble(float_fractional_part(R1, R2), S, S) :- gen('X ', R2, '=num_frac(', R1, ');\n').
assemble(float_integer_part(R1, R2), S, S) :- gen('X ', R2, '=num_int(', R1, ');\n').
assemble(floor(R1, R2), S, S) :- gen('X ', R2, '=num_floor(', R1, ');\n').
assemble(log(R1, R2), S, S) :- gen('X ', R2, '=num_log(', R1, ');\n').
assemble(round(R1, R2), S, S) :- gen('X ', R2, '=num_round(', R1, ');\n').
assemble(sign(R1, R2), S, S) :- gen('X ', R2, '=num_sign(', R1, ');\n').
assemble(sin(R1, R2), S, S) :- gen('X ', R2, '=num_sin(', R1, ');\n').
assemble(tan(R1, R2), S, S) :- gen('X ', R2, '=num_tan(', R1, ');\n').
assemble(sqrt(R1, R2), S, S) :- gen('X ', R2, '=num_sqrt(', R1, ');\n').
assemble(truncate(R1, R2), S, S) :- gen('X ', R2, '=num_truncate(', R1, ');\n').
assemble(negate(R1, R2), S, S) :- gen('X ', R2, '=num_negate(', R1, ');\n').
assemble(random(R1, R2), S, S) :- gen('X ', R2, '=num_random(', R1, ');\n').
assemble(clock(R1), S, S) :- gen('X ', R1, '=num_clock();\n').
assemble(integer(R), S, S) :- gen('if(!is_FIXNUM(deref(', R, '))) FAIL;\n').
assemble(number(R), S, S) :- gen('if(!is_number(deref(', R, '))) FAIL;\n').
assemble(var(R), S, S) :- gen('if(!is_variable(deref(', R, '))) FAIL;\n').
assemble(nonvar(R), S, S) :- gen('if(is_variable(deref(', R, '))) FAIL;\n').
assemble(atom(R), S, S) :- gen('if(!is_atom(deref(', R, '))) FAIL;\n').
assemble(atomic(R), S, S) :- gen('if(!is_atomic(deref(', R, '))) FAIL;\n').
assemble(pair(R), S, S) :- gen('if(!is_pair(deref(', R, '))) FAIL;\n').
assemble(compound(R), S, S) :- gen('if(!is_compound(deref(', R, '))) FAIL;\n').
assemble(float(R), S, S) :- gen('if(!is_float(deref(', R, '))) FAIL;\n').
assemble(is_stream(R), S, S) :- gen('if(!is_stream(deref(', R, '))) FAIL;\n').
assemble(db_reference(R), S, S) :- gen('if(!is_dbreference(deref(', R, '))) FAIL;\n').
assemble(foreign_pointer(R), S, S) :- gen('if(!is_pointer(deref(', R, '))) FAIL;\n').
assemble(check_nonvar(I), S, S) :- gen('CHECK_NONVAR(', I, ');\n').
assemble(structure(R, N, A, _), S1, S2) :-
	gensym('T', T, S1, S2),
	gen('X ', T, '=deref(', R, ');\n'), % deref just once
	gen('if(!is_structure(', T, ')||objsize(', T, ')!='),
	A2 is A + 1,		% add functor name
	gen(A2, '||slot_ref(', T, ',0)!=literal_', N),
	gen(') FAIL;\n').
assemble(arg(R1, I, R2), S, S) :-
	gen('X ', R2, '=slot_ref(deref(', R1),
	gen('),', I, ');\n').
assemble(term_less(R1, R2), S, S) :- gen('if(compare_terms(deref(', R1, '),deref(', R2, ')) <= 0) FAIL;\n').
assemble(term_not_less(R1, R2), S, S) :- gen('if(compare_terms(deref(', R1, '),deref(', R2, ')) > 0) FAIL;\n').
assemble(global_ref(NAME, R), S, S) :-
	mangle_name(NAME, MNAME),
	gen('X ', R, '=GLOBAL_REF(', MNAME, ');\n').
assemble(global_set(NAME, R), S, S) :-
	mangle_name(NAME, MNAME),
	gen('GLOBAL_SET(', MNAME, ',', R, ');\n').
assemble(simple_test(L), S, S) :-
	gen('#undef FAIL\n#define FAIL QUASI_FAILURE(', L, ')\n').
assemble(end_simple_test, S, S) :-
	gen('#undef FAIL\n#define FAIL FAILURE\n').
assemble(trace_off, S, S) :-
	(recorded(trace_libraries, yes); gen('#define debugging 0\n')).
assemble(switch_on_integer(L), S, S) :- gen('if(is_FIXNUM(A[0])) goto ', L, ';\n').
assemble(switch_on_var(L), S, S) :- gen('if(is_VAR(A[0])) goto ', L, ';\n').
assemble(switch_on_null(L), S, S) :- gen('if(A[0]==END_OF_LIST_VAL) goto ', L, ';\n').
assemble(switch_on_atom(L), S, S) :- gen('if(is_SYMBOL(A[0])) goto ', L, ';\n').
assemble(switch_on_float(L), S, S) :- gen('if(is_FLONUM(A[0])) goto ', L, ';\n').
assemble(switch_on_pair(L), S, S) :- gen('if(is_PAIR(A[0])) goto ', L, ';\n').
assemble(switch_on_structure(L), S, S) :- gen('if(is_STRUCTURE(A[0])) goto ', L, ';\n').
assemble(switch_and_dispatch_on_atom(ENTRIES, TLEN, LX), S, S) :-
	gen('if(!is_SYMBOL(A[0])) goto ', LX, ';\n'),
	gen('static SYMBOL_DISPATCH dt_', LX, '[]={'),
	assemble_atom_dispatch(0, TLEN, ENTRIES, 'NULL'),
	gen('};\nDISPATCH_ON_SYMBOL(dt_', LX, ','),
	gen(LX, ',', TLEN),
	gen(');\n', LX, ':\n').
assemble(switch_and_dispatch_on_structure(ENTRIES, TLEN, LX), S, S) :-
	gen('if(!is_STRUCTURE(A[0])) goto ', LX, ';\n'),
	gen('static STRUCTURE_DISPATCH dt_', LX, '[]={'),
	assemble_structure_dispatch(0, TLEN, ENTRIES, 'NULL'),
	gen('};\nDISPATCH_ON_STRUCTURE(dt_', LX, ','),
	gen(LX, ',', TLEN),
	gen(');\n', LX, ':\n').
assemble(dispatch_on_integer(TABLE), S, S) :-
	gen('switch(fixnum_to_word(A[0])){\n'),
	forall(member(N/L, TABLE), gen('case ', N, ':goto ', L, ';\n')),
	gen('}').
assemble(suspend(R1, L), S, S) :-
	gen('saved_state.result=', R1, ';\nsaved_state.P=&&', L, ';\n'),
	gen('goto suspend;\n', L, ':\n', R1, '=saved_state.result;\n').
assemble(call_triggered(L), S, S) :-
	gen('CALL_TRIGGERED(', L, ');\n').
assemble(OP, _, _) :-
	error(['invalid pseudo instruction: ', OP]).


%% assemble list of arguments

assemble_arguments([], _).
assemble_arguments([X|MORE], I) :-
	gen('*(arg_top++)=', X, ';\n'),
	I2 is I + 1,
	assemble_arguments(MORE, I2).


%% assemble dispatch table

assemble_integer_dispatch(I, I, _, _).
assemble_integer_dispatch(I, LEN, [I-(INT/LABEL)|MORE], NULL) :-
	!,
	gen('{', INT, ','), gen_label_or_index(LABEL), gen('},'),
	I2 is I + 1,
	assemble_integer_dispatch(I2, LEN, MORE, NULL).
assemble_integer_dispatch(I, LEN, ENTRIES, NULL) :-
	gen('{0,', NULL, '},'),
	I2 is I + 1,
	assemble_integer_dispatch(I2, LEN, ENTRIES, NULL).

assemble_atom_dispatch(I, I, _, _).
assemble_atom_dispatch(I, LEN, [I-(ATOM/LABEL)|MORE], NULL) :-
	!,
	mangle_name(ATOM, NAME),
	gen('{(X)SYMBOL', NAME, ','), gen_label_or_index(LABEL), gen('},'),
	I2 is I + 1,
	assemble_atom_dispatch(I2, LEN, MORE, NULL).
assemble_atom_dispatch(I, LEN, ENTRIES, NULL) :-
	gen('{NULL,', NULL, '},'),
	I2 is I + 1,
	assemble_atom_dispatch(I2, LEN, ENTRIES, NULL).

assemble_structure_dispatch(I, I, _, _).
assemble_structure_dispatch(I, LEN, [I-((N/A)/LABEL)|MORE], NULL) :-
	!,
	mangle_name(N, NAME),
	gen('{(X)SYMBOL', NAME, ',', A, ','), gen_label_or_index(LABEL), gen('},'),
	I2 is I + 1,
	assemble_structure_dispatch(I2, LEN, MORE, NULL).
assemble_structure_dispatch(I, LEN, ENTRIES, NULL) :-
	gen('{NULL,0,', NULL, '},'),
	I2 is I + 1,
	assemble_structure_dispatch(I2, LEN, ENTRIES, NULL).

gen_label_or_index(X) :- integer(X), !, gen(X).
gen_label_or_index(X) :- gen('&&', X).


%% generate literal data

assemble_literals(S1, S2) :-
	recorded(literal, [INDEX|TERM], REF), erase(REF),
	generate_static_literal(INDEX, TERM, S1, S),
	!, assemble_literals(S, S2). % force tail call
assemble_literals(S, S).

generate_static_literal(I, X, S, S) :-
	integer(X),
	default_setting(literal_fixnum_range, LOW - HIGH),
	X >= LOW, X =< HIGH,
	gen('#define literal_', I, ' word_to_fixnum(', X, ')\n').
generate_static_literal(I, X, S, S) :-
	number(X), !,
	gen('static FLONUM_BLOCK lb', I, '={FLONUM_TAG|sizeof(XFLOAT),', X, '};\n'),
	gen('#define literal_', I, ' &lb', I, '\n').
generate_static_literal(I, [], S, S) :-
	!, gen('#define literal_', I, ' END_OF_LIST_VAL\n').
generate_static_literal(I, X, S, S) :-
	atom(X), !,
	mangle_name(X, NAME),
	gen('#ifndef SYMBOL', NAME, '\n'),
	name(X, STRING),
	length(STRING, LEN),
	gen('static STRING_BLOCK lbs', I, '={STRING_TAG|(', LEN, '+1),{'),
	generate_data_list(STRING),
	gen('0}};\nstatic SYMBOL_BLOCK lb', I, '={SYMBOL_TAG|3,(X)&lbs', I),
	atom_hash(X, HASH),
	gen(',PREVIOUS_SYMBOL,word_to_fixnum(', HASH),
	gen(')};\n#undef PREVIOUS_SYMBOL\n#define PREVIOUS_SYMBOL (X)&lb', I),
	gen('\n#define literal_', I, ' &lb', I, '\n'),
	gen('#define SYMBOL', NAME, ' literal_', I, '\n#else\n'),
	gen('#define literal_', I, ' SYMBOL', NAME, '\n#endif\n').
generate_static_literal(I, [X|Y], S1, S2) :-
	!,
	gensym('p', CAR, S1, S3),
	generate_static_literal(CAR, X, S3, S4),
	gensym('p', CDR, S4, S5),
	generate_static_literal(CDR, Y, S5, S2),
	gen('static BLOCK lb', I, '={PAIR_TAG|2,{literal_', CAR),
	gen(',literal_', CDR, '}};\n'),
	gen('#define literal_', I, ' &lb', I, '\n').
generate_static_literal(I, X, S1, S2) :-
	X =.. [F|ARGS], !,
	gensym('f', IF, S1, S3),
	generate_static_literal(IF, F, S3, S4),
	generate_static_literals(ARGS, 1, IF, IS, S4, S2),
	length(ARGS, ARITY),
	gen('static BLOCK lb', I, '={(STRUCTURE_TAG|', ARITY),
	gen(')+1,{literal_', IF, ','),
	generate_data_list(IS),
	gen('}};\n#define literal_', I, ' &lb', I, '\n').

generate_static_literals([], _, _, [], S, S).
generate_static_literals([X|MORE], I, IF, [IS|IMORE], S1, S2) :-
	atomic_list_concat([IF, '_', I], IS1),
	atomic_list_concat(['literal_', IS1], IS),
	generate_static_literal(IS1, X, S1, S),
	I2 is I + 1,
	generate_static_literals(MORE, I2, IF, IMORE, S, S2).


% output helpers

generate_data_list([]).
generate_data_list([X|Y]) :- gen(X,','), generate_data_list(Y).

generate_foreign_arguments([]).
generate_foreign_arguments([X]) :- gen(',deref(', X, ')').
generate_foreign_arguments([X|MORE]) :-
	gen(',deref(', X, ')'),
	generate_foreign_arguments(MORE).

generate_slot_inits(_, _, []).
generate_slot_inits(R, I, [X|M]) :-
	gen('SLOT_INIT(', R, ',', I, ','),
	gen(X, ');\n'),
	I2 is I + 1,
	generate_slot_inits(R, I2, M).

generate_literal_list([]).
generate_literal_list([N|R]) :-
	gen('literal_', N, ','),
	generate_literal_list(R).