%%% process toplevel forms


compile_file(FILE) :-
	initial_state(STATE),
	see(FILE), !,
	skip_shebang,
	process_input([], [], _, STATE).
compile_file(FILE) :-
	error(['compilation of ', FILE, ' failed.']).

compile_file_finished(_) :-
	recorded(xref_mode, yes),
	!,
	emit_xref_information.
compile_file_finished(STATE) :-
	process_discontiguous_code(STATE).
compile_file_finished(STATE) :-
	export_public_predicates,
	process_boilerplate_code(STATE).
compile_file_finished(STATE) :-
	process_initialization_goals(STATE).
compile_file_finished(STATE) :-
	mark_unused_predicates,
	check_unresolved_calls,
	( recorded(xref_mode, all)
	-> emit_xref_information
	; recorded(output_file, OUTFILE),
	  (recorded(show_intermediate_code, yes) -> show_intermediate_code
	  ; assemble_file(OUTFILE, STATE)
	  )
	).


%% process next input, collecting a "block" of clauses of a given name/arity

% no more input, read next
process_input([], BLOCK, NA, STATE) :-
	'$read1'(EXPR),
	!,
	process_input([EXPR], BLOCK, NA, STATE).

% end of file reached, compile block, if not empty
process_input([end_of_file], BLOCK, NA, STATE) :-
	open_file_stack(INFILE, STATE2, STATE), % note order
	seen,
	see(INFILE),
	!,
	process_input([], BLOCK, NA, STATE2).
process_input([end_of_file], [], _, STATE) :-
	!,
	compile_file_finished(STATE).
process_input([end_of_file], BLOCK, NA, STATE1) :-
	compile_block(NA, BLOCK, STATE1, STATE2),
	!,
	compile_file_finished(STATE2).

% detect DCG rules and expand
process_input([(HEAD --> BODY)|MORE], BLOCK, NA, STATE) :-
        dcg_rule((HEAD --> BODY), EXPANSION),
	!,
	process_input([EXPANSION|MORE], BLOCK, NA, STATE).

% marked as discontiguous? just record
process_input([CLAUSE|MORE], BLOCK, NA, STATE) :-
	clause_functor(CLAUSE, CN, CA),
	recorded(discontiguous_predicate, CN/CA),
	recordz(discontiguous_clause, CLAUSE),
	( BLOCK \== []
	-> compile_block(NA, BLOCK, STATE, STATE2)
	; STATE2 = STATE
	),
	!,
	process_input(MORE, [], _, STATE2).

% matches N/A? add and continue
process_input([(HEAD :- BODY)|MORE], BLOCK, NAME/ARITY, STATE) :-
	functor(HEAD, NAME, ARITY),
	!,			   
	process_input(MORE, [(HEAD :- BODY)|BLOCK], NAME/ARITY, STATE).

% otherwise compile current block and start new block
process_input([(HEAD :- BODY)|MORE], BLOCK, NA, STATE) :-
	functor(HEAD, NAME, ARITY),
	compile_block(NA, BLOCK, STATE, STATE2),
	!,
	process_input(MORE, [(HEAD :- BODY)], NAME/ARITY, STATE2).

% detect declarations
process_input([(:- DECL)|MORE], [], _, STATE) :-
	!, 
	process_directive(DECL, STATE, STATE2),
	process_input(MORE, [], _, STATE2).
process_input([(:- DECL)|MORE], BLOCK, NA, S) :-
        compile_block(NA, BLOCK, S, S1),
	!,
	process_directive(DECL, S1, S2),
	process_input(MORE, [], _, S2).

% the same for facts
process_input([HEAD|MORE], BLOCK, NAME/ARITY, STATE) :-
	functor(HEAD, NAME, ARITY),
	!,
	process_input(MORE, [HEAD|BLOCK], NAME/ARITY, STATE).

process_input([HEAD|MORE], BLOCK, NA, STATE) :-
	functor(HEAD, NAME, 0),
	!,
	compile_block(NA, BLOCK, STATE, STATE2),
	process_input(MORE, [HEAD], NAME/0, STATE2).

% otherwise invalid - if there is an active block, compile it first
process_input(INPUT, [C|CR], NA, STATE) :-
	compile_block(NA, [C|CR], STATE, STATE2),
	!,
	process_input(INPUT, [], _, STATE2).
process_input([EXPR|_], _, _, _) :-
	error(['invalid clause: ', EXPR]).


%% process a directive (declaration)

process_directive((DECL1, DECL2), S1, S2) :-
	process_directive(DECL1, S1, S),
	process_directive(DECL2, S, S2).
process_directive(DECL, _, _) :-
	recorded(xref_mode, yes),
	recordz(directive, DECL),
	fail.
process_directive(initialization(GOAL), STATE, STATE) :-
	( recorded(initialization_goal, OLD, REF), erase(REF) ->
	  recorda(initialization_goal, (OLD, GOAL))
	; recorda(initialization_goal, GOAL)).
process_directive(pre_initialization(GOAL), STATE, STATE) :-
	( recorded(pre_initialization_goal, OLD, REF), erase(REF) ->
	  recorda(pre_initialization_goal, (OLD, GOAL))
	; recorda(pre_initialization_goal, GOAL)).
process_directive(include(FNAME), STATE1, STATE2) :-
	locate_file(FNAME, REALNAME),
	seeing(CURRENT),
	recordz(included, REALNAME),
	open_file_stack(CURRENT, STATE1, STATE2),
	message(['% including ', REALNAME]),
	see(REALNAME).
process_directive(ensure_loaded(FNAME), S1, S2) :-
	locate_file(FNAME, REALNAME),
	( recorded(included, REALNAME)
	-> S2 = S1
	; process_directive(include(REALNAME), S1, S2)
	).
process_directive(global_variable(NAME), S, S) :-
	mangle_name(NAME, MNAME),
	recordz(global_variables, MNAME).
process_directive(verbatim(STR), S, S) :-
	recordz(verbatim_code, STR).
process_directive(determinate(PI), S, S) :-
	mark_predicate_indicators(PI, determinate_predicate).
process_directive(op(P, A, N), S, S) :- op(P, A, N).
process_directive(meta_predicate(META), S, S) :-
	register_predicate_annotation(META, meta_signature).
process_directive(public(PI), S, S) :-
	mark_predicate_indicators(PI, public_predicate).
process_directive(discontiguous(PI), S, S) :-
	mark_predicate_indicators(PI, discontiguous_predicate).
process_directive(mode(MODES), S, S) :-
	register_predicate_annotation(MODES, mode_declaration).
process_directive(trace_libraries, S, S) :-
	recordz(trace_libraries, yes).
process_directive(compress_facts, S, S) :-
	recordz(compress_facts, yes).
process_directive(DECL, STATE, STATE) :-
	error(['unrecognized directive: ', DECL]).


%% register comma-separated list of predicates with annotated arguments

register_predicate_annotation((X, Y), LABEL) :-
	register_predicate_annotation(X, LABEL),
	register_predicate_annotation(Y, LABEL).
register_predicate_annotation(X, LABEL) :-
	functor(X, NAME, ARITY),
	X =.. [_|ANN],
	recordz(LABEL, info(NAME, ARITY, ANN)).
register_predicate_annotation(X, _) :-
	error(['invalid predicate annotation: ', X]).


%% register list of predicate-indicators with some mark

mark_predicate_indicators((X, Y), MARK) :-
	mark_predicate_indicators(X, MARK),
	mark_predicate_indicators(Y, MARK).
mark_predicate_indicators(PI, MARK) :-
	(canonical_pi(PI, N/A); error(['invalid predicate-indicator: ', PI])),
	recordz(MARK, N/A).


%% compile a block of clauses

compile_block(NA, BLOCK, STATE1, STATE2) :-
	reverse(BLOCK, RBLOCK),
	compile_clauses(NA, RBLOCK, STATE1, STATE2).


%% list generated intermediate code

show_intermediate_code :-
	recorded(code, OP, REF), erase(REF),
	writeq(OP), put(46), nl,
	fail.
show_intermediate_code.


%% add discontiguous predicates

process_discontiguous_code(STATE) :-
	recorded(discontiguous_clause, CLAUSE),
	clause_functor(CLAUSE, N, A),
	recorded(discontiguous_predicate, N/A, REF),
	erase(REF),		% or this loops!
	findall(C, (recorded(discontiguous_clause, C, R),
		    clause_functor(C, N, A),
		    erase(R)),
		CLAUSES),
	!,
	process_input(CLAUSES, [], _, STATE).


%% add clauses for boilerplate code and (pre-)initialization goals

process_boilerplate_code(STATE) :-
	emit(trace_off),
	findall(B, (recorded(boilerplate, B, REF), erase(REF)), BOILERPLATE),
	BOILERPLATE \== [],
	!,
	process_input(BOILERPLATE, [], _, STATE).

process_initialization_goals(STATE) :-
	\+recorded(initialization_done, _),
	(recorded(initialization_goal, GOAL); GOAL = main),
	(recorded(pre_initialization_goal, IGOAL); IGOAL = true),
	(recorded(public_predicate, _), PGOAL = '$init_public'; PGOAL = true),
	recorda(initialization_done, true),
	default_setting(entry_point, EP),
	register_call(?/?, EP/0),
	!,
	process_input([(EP :- IGOAL, PGOAL, GOAL)], [], _, STATE).


%% list unresolved calls

check_unresolved_calls :-
	recorded(unresolved, _),
	display(user_error, '\nUnresolved predicate calls:\n\n'),
	report_unresolved_calls.
check_unresolved_calls.

report_unresolved_calls :-
	recorded(unresolved, N/A),
	tab(user_error, 2), write(user_error, N/A), nl(user_error), fail.
report_unresolved_calls :-
	recorded(unresolved, _),
	nl(user_error),
	halt(1).
report_unresolved_calls.


%% add initialization-code to record public predicates

export_public_predicates :-
	recorded(public_predicate, NA),
	\+recorded(defined, NA),
	display(user_error, 'WARNING: public predicate was not defined: '),
	writeq(user_error, NA),
	nl(user_error),
	fail.
export_public_predicates :-
	findall(INIT, (recorded(defined, N/A), % bind possibly unbound arity
		       recorded(public_predicate, N/A),
		       INIT = ('$predicate_address'(N/A, PTR), recordz('$public', p(N, A, PTR))) ),
		INITS),
	combine_comma_separated_goals(INITS, G),
	add_boilerplate('$init_public', ('$init_public' :- G)).
