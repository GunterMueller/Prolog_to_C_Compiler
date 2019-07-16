%%% compile.pl


%% compile a set of clauses

compile_clauses(';'/2, _, _, _) :-
	error(['attempt to redefine builtin predicate: ', ';'/2]).
compile_clauses(NAME/ARITY, CLAUSES, S1, S2) :-
	gen_label(L1, S1, S3),
	emit(enter(NAME, ARITY, L1)),
	compile_mode_checks(NAME, ARITY),
	register_defined_predicate(NAME/ARITY),
	build_index_to_type_map(CLAUSES, 1, MAP),
	( ARITY > 0,
	  recorded(compress_facts, yes),
	  length(CLAUSES, N),
	  default_setting(fact_block_threshold, T),
	  N >= T,
	  fact_block(CLAUSES)
	-> compile_fact_block(NAME/ARITY, CLAUSES, MAP, S3, S2)
	; compile_clause_list(CLAUSES, NAME/ARITY, MAP, det, S3, S2)
	).

compile_clause_list([CLAUSE], NAME/ARITY, [I/_], D1, S1, S2) :-
	clause_label(NAME, ARITY, I, L1),
	secondary_clause_label(NAME, ARITY, I, L2),
	emit(label(L1), label(L2)),
	(I =:= 1; emit(redo)),
	(D1 == det -> LAST = detlast; LAST = last),
	compile_clause(CLAUSE, NAME/ARITY, I, LAST, D1, D, S1, S2),
	mark_as_determinate(NAME/ARITY, D).
compile_clause_list([CLAUSE|MORE], NAME/ARITY, [I/T|MAP], D1, S1, S2) :-
	clause_label(NAME, ARITY, I, L1),
	emit(label(L1)),
	( I =:= 1
	-> compile_clause_indexing(NAME, ARITY, [I/T|MAP], S1, S3)
	; S3 = S1
	),
	secondary_clause_label(NAME, ARITY, I, L2),
	emit(label(L2)),
	compile_clause(CLAUSE, NAME/ARITY, I, notlast, D1, D2, S3, S4),
	!,
	compile_clause_list(MORE, NAME/ARITY, MAP, D2, S4, S2).

% rule
compile_clause((HEAD :- BODY), NAME/ARITY, I, LAST, D1, D, S1, S2) :-
        show_compiled_clause((HEAD :- BODY)),
	!,			% avoid match of next clause (fact)
	I2 is I + 1,
	clause_label(NAME, ARITY, I2, L),
	compile_redo(LAST, L),
	index_variables([HEAD, BODY], VARS, NONSINGLETONS),
	length(VARS, N),
	emit(environment(N)),
	compile_head(HEAD, NONSINGLETONS, BOUND, S1, S3),
	gen_label(L1, S3, S4),
	emit(call_triggered(L1)),
	compile_body(BODY, NAME/ARITY, LAST, BOUND, D1, D, S4, S2).
% fact
compile_clause(HEAD, NA, I, M, D1, D, S1, S2) :-
	compile_clause((HEAD :- true), NA, I, M, D1, D, S1, S2).


%% compile "block" of facts, if all arguments are ground

fact_block([]).
fact_block([(_ :- _)|_]) :- !, fail.
fact_block([H|R]) :- ground_term(H), !, fact_block(R).

compile_fact_block(NA, CLAUSES, MAP, S1, S) :-
	scan_indexing_types(MAP, [], DMAP),
	type_map_first_indices(DMAP, TABLE),
	compile_facts(CLAUSES, DATA, S1, S2),
	length(DATA, N1), DATA = [A1|_], length(A1, N2), N3 is N1 * N2,
	message(['% compressed fact block ', NA, ': ', N1/N3]),
	( build_dispatch_table(NA, integer, integer_table_index_threshold, DMAP, ITABLE,
			       TILEN, DMAP2)
	; DMAP2 = DMAP, TILEN = 0, ITABLE = none
	),
	( build_dispatch_table(NA, atom, atom_table_index_threshold, DMAP2, ATABLE, TALEN,
			       DMAP3)
	; DMAP3 = DMAP2, TALEN = 0, ATABLE = none
	),
	( build_dispatch_table(NA, structure, structure_table_index_threshold, DMAP3,
			       STABLE, TSLEN, _)
	; TSLEN = 0, STABLE = none
	),
	gen_label(L, S2, S),
	emit(unify_facts(L, DATA, TABLE, TILEN, ITABLE, TALEN, ATABLE, TSLEN, STABLE)).

compile_facts([], [], S, S).
compile_facts([F|R], [D|ATA], S1, S) :-
	F =.. [_|ARGS],
	register_literals(ARGS, D, S1, S2),
	compile_facts(R, ATA, S2, S).


%% show clauses as they are compiled
show_compiled_clause(CLAUSE) :-
	recorded(show_compiled_clauses, yes),
	display('% '), writeq(CLAUSE), put(46), nl.
show_compiled_clause(_).


%% perform CP-handling for a particular clause-position (no CP needed in last clause)
compile_redo(notlast, L) :- emit(set_redo(L)).
compile_redo(last, _) :- emit(no_redo).
compile_redo(detlast, _) :- emit(no_redo).


%% generate clause labels from name/arity + index, and 2nd label for dispatch-target

clause_label(N, A, I, L) :-
	mangle_name(N, MN),
	atomic_list_concat([MN, '$', A, '_', I], L).

secondary_clause_label(N, A, I, L) :-
	mangle_name(N, MN),
	atomic_list_concat([MN, '$', '$', A, '_', I], L).


%% compile head-unification

compile_head(HEAD, _, [], S, S) :-
	functor(HEAD, _, 0).	% nothing to do
compile_head(HEAD, _, [], S1, S2) :-
	ground_term(HEAD),	% special case for head, where all arguments are ground
	HEAD =.. [_|ARGS],
	default_setting(unify_argument_list_threshold, T),
	functor(HEAD, _, ARITY), ARITY >= T,
	register_literals(ARGS, LS, S1, S3),
	gen_label(LBL, S3, S2),
	emit(unify_args(LBL, LS)).	
compile_head(HEAD, NS, BOUND, S1, S2) :-
	functor(HEAD, NAME, ARITY),
	HEAD =.. [_|ARGS],
	get_head_modes(NAME, ARITY, MODES),
	compile_unification(ARGS, MODES, NS, 0, [], BOUND, S1, S2).

get_head_modes(NAME, ARITY, MODES) :-
	recorded(mode_declaration, info(NAME, ARITY, MODES)).
get_head_modes(_, _, none).

	       
%% compile unification of head-argument

compile_unification([], _, _, _, BOUND, BOUND, S, S).
compile_unification([ARG|MORE], ['+'|MODES], NS, INDEX, BOUND1, BOUND2, S1, S2) :-
	compile_ground_unification(ARG, NS, INDEX, BOUND1, BOUND, S1, S),
	INDEX2 is INDEX + 1,
	!,
	compile_unification(MORE, MODES, NS, INDEX2, BOUND, BOUND2, S, S2).
compile_unification([ARG|MORE], MODES, NS, INDEX, BOUND1, BOUND2, S1, S2) :-
	get_next_head_modes(MODES, MODES2),
	compile_unification1(ARG, NS, INDEX, BOUND1, BOUND, S1, S),
	INDEX2 is INDEX + 1,
	!,
	compile_unification(MORE, MODES2, NS, INDEX2, BOUND, BOUND2, S, S2).

get_next_head_modes([_|MODES], MODES).
get_next_head_modes(MODES, MODES).


%% compile unification of non-var head-argument that is known
%% (declared) to be ground

compile_ground_unification(X, _, I, B, B, S1, S2) :-
	atomic(X),
	gensym('T', T1, S1, S3),
	gensym('T', T2, S3, S4),
	register_literal(X, LIT, S4, S2),
	emit(argument(I, T1), literal(LIT, T2, X)),
	( (integer(X); atom(X))
	-> emit(eq(T1, T2))
	; emit(identical(T1, T2))
	).
compile_ground_unification(X, NS, I, B1, B2, S1, S2) :-
	\+indexed_variable(X, _),
	functor(X, NAME, ARITY),
	X =.. [_|ARGS],
	gensym('T', T1, S1, S3),
	emit(argument(I, T1)),
	( NAME == '.'
	-> emit(pair(T1)),
	  I1 = 0,		% start index
	  S4 = S3
	; register_literal(NAME, N, S3, S4),
	  emit(structure(T1, N, ARITY, NAME/ARITY)),
	  I1 = 1		% skip functor name
	),
	compile_argument_unifications(ARGS, I1, T1, NS, B1, B2, S4, S2).

compile_argument_unifications([], _, _, _, B, B, S, S).
compile_argument_unifications([ARG|MORE], I, T, NS, B1, B2, S1, S2) :-
	indexed_variable(ARG, N),
	\+memberchk(N, NS),	% singleton? then do nothing
	I2 is I + 1,
	compile_argument_unifications(MORE, I2, T, NS, B1, B2, S1, S2).
compile_argument_unifications([ARG|MORE], I, T, NS, B1, B2, S1, S2) :-
	gensym('T', TA, S1, S3),
	gensym('T', TB, S3, S4),
	emit(arg(T, I, TA)),
	compile_term_for_unification(ARG, TB, B1, B3, S4, S5),
	%% very subtle: switching TA and TB will break bagof/setof,
	%% due to order of binding - it is important that '$unbound_variables'/2
	%% in lib/findall.pl returns the original variables!
	emit(unify(TB, TA)),
	I2 is I + 1,
	!,
	compile_argument_unifications(MORE, I2, T, NS, B3, B2, S5, S2).


%% distinguish cases: singleton or bound/unbound variable or term (either constant or containing variable)
compile_unification1(X, NS, _, B, B, S, S) :-
	indexed_variable(X, N),
	\+memberchk(N, NS).	% singleton? then do nothing
compile_unification1(X, _, INDEX, BOUND, BOUND2, S1, S2) :-
	indexed_variable(X, N),
	gensym('T', T1, S1, S3),
	( memberchk(N, BOUND) 	% already bound?
	-> (gensym('T', T2, S3, S2),
	    BOUND2 = BOUND,
	    emit(local(N, T1), argument(INDEX, T2), unify(T1, T2))
	   )
	; ( S2 = S3,
	    BOUND2 = [N|BOUND],
	    emit(argument(INDEX, T1), assign(N, T1))
	  )
	).
compile_unification1(TERM, _, INDEX, BOUND1, BOUND2, S1, S2) :-
	gensym('T', T1, S1, S3),
	gensym('T', T2, S3, S4),
	compile_term_for_unification(TERM, T1, BOUND1, BOUND2, S4, S2),
	emit(argument(INDEX, T2), unify(T1, T2)).

%% compile term, for unification, or for calls
compile_term_for_unification(X, DEST, BOUND, BOUND2, S, S) :-
	indexed_variable(X, N),
	( member(N, BOUND)	% already bound?
	-> ( emit(local(N, DEST)),
	     BOUND2 = BOUND
	   )
	; ( BOUND2 = [N|BOUND],
	    emit(make_variable(DEST), assign(N, DEST))
	  )
	).
compile_term_for_unification(X, DEST, BOUND, BOUND, S1, S2) :-
	ground_term(X), 	% literal term not containing variables?
	register_literal(X, N, S1, S2),
	emit(literal(N, DEST, X)).
compile_term_for_unification([X|Y], DEST, BOUND1, BOUND2, S1, S2) :-
	compile_term_arguments([X, Y], [], [CAR, CDR], BOUND1, BOUND2, S1, S2),
	emit(make_pair(CAR, CDR, DEST)).
compile_term_for_unification(X, DEST, BOUND1, BOUND2, S1, S2) :-
	X =.. LIST,
	compile_term_arguments(LIST, [], DLIST, BOUND1, BOUND2, S1, S2),
	emit(make_term(DLIST, DEST)).

compile_meta_term_for_unification(SPEC, NA, G, DEST, B1, B2, S1, S2) :-
	gensym('$meta_call', P, S1, S3),
	goals_and_variables(G, VLIST, G2, IARGS),
	map_second(VLIST, VARGS),
	HEAD =.. [P|VARGS],
	adjust_meta_call(SPEC, NA, HEAD, G2, HEAD2, G22),
	functor(HEAD2, _, LEN),
	add_boilerplate(P, (HEAD2 :- G22)),
	gensym('T', T1, S3, S4),
	gensym('T', T2, S4, S5),
	gensym('T', T3, S5, S6),
	register_literal('$meta_call', NLIT, S6, S7), % functor-name
	register_call(NA, P/LEN),
	emit(literal(NLIT, T1, '$meta_call'), predicate_address(P, LEN, T2)), % pred-ptr
	compile_term_for_unification(IARGS, T3, B1, B2, S7, S2), % variable-list
	emit(make_term([T1, T2, T3], DEST)).

% adjust synthesized meta-predicate and goal-invocation according to argument spec
adjust_meta_call('^', _, H, _^G, H2, G2) :-
	%% quantified variables are currently lost
	adjust_meta_call('^', _, H, G, H2, G2).
adjust_meta_call('^', _, H, G, H, G).
adjust_meta_call(N, NA, H, G, H2, G2) :-
	integer(N),
	(N >= 0; error(['invalid meta-predicate argument specification ', N, ' for ', NA])),
	length(VARS, N),
	H =.. [HN|HARGS], G =.. [GN|GARGS],
	append(HARGS, VARS, HARGS2), append(GARGS, VARS, GARGS2),
	H2 =.. [HN|HARGS2], G2 =.. [GN|GARGS2].
adjust_meta_call(_, _, H, G, H, G). % any other is ignored

% compile list of arguments, putting elements into registers
compile_term_arguments([], DL, RDL, B, B, S, S) :-
	reverse(DL, RDL).
compile_term_arguments([X|MORE], DL1, DL2, B1, B2, S1, S2) :-
	gensym('T', T, S1, S3),
	compile_term_for_unification(X, T, B1, B3, S3, S4),
	compile_term_arguments(MORE, [T|DL1], DL2, B3, B2, S4, S2).

% the same, but with signature of meta-predicate
compile_meta_arguments([], _, _, DL, RDL, B, B, S, S) :-
	reverse(DL, RDL).
compile_meta_arguments([X|MORE], NA, [SPEC|SIG], DL1, DL2, B1, B2, S1, S2) :-
	gensym('T', T, S1, S3),
	( (integer(SPEC); SPEC == '^')
	-> compile_meta_term_for_unification(SPEC, NA, X, T, B1, B3, S3, S4)
	; compile_term_for_unification(X, T, B1, B3, S3, S4)
	),
	compile_meta_arguments(MORE, NA, SIG, [T|DL1], DL2, B3, B2, S4, S2).
		       

%% compile body

compile_body(BODY, N/A, LAST, BOUND, D1, D, S1, S2) :-
	(LAST == last; LAST == detlast -> DET = det; DET = nondet),
	compile_body_expression(BODY, N/A, tail, LAST/DET, LD2, BOUND, _, S1, S3),
	gen_label(L, S3, S2),
	( ( LD2 = _/det; determinate_builtin(N, A) )
	-> emit(determinate_exit),
	  D = D1
	; emit(exit(L)),
	  D = nondet
	).


%% compile expression occuring in clause body

% first, try macros
compile_body_expression(TERM, NA, TAIL, D1, D2, B1, B2, S1, S2) :-
	macro(TERM, EXPANSION),
	TERM \= EXPANSION,	% handle macro that just adds boilerplate (e.g. "autoload" like)
	!,
	compile_body_expression(EXPANSION, NA, TAIL, D1, D2, B1, B2, S1, S2).

% conjunction
compile_body_expression((X, Y), NA, TAIL, D1, D2, B1, B2, S1, S2) :-
	compile_body_expression(X, NA, nontail, D1, D, B1, B, S1, S),
	!,
	compile_body_expression(Y, NA, TAIL, D, D2, B, B2, S, S2).

% if-then-else
compile_body_expression((X -> Y; Z), NA, TAIL, D1, D2, B1, B2, S1, S2) :-
	simple_test(X),
	% test-expression is "simple"
	gen_label(L1, S1, S3),
	gen_label(L2, S3, S4),
	emit(simple_test(L1)),
	compile_body_expression(X, NA, nontail, D1, _, B1, B3, S4, S5),
	emit(end_simple_test),
	collect_indexed_variables(Y, BY1), subtract(BY1, B3, BY),
	collect_indexed_variables(Z, BZ1), subtract(BZ1, B3, BZ),
	compile_body_expression(Y, NA, TAIL, D1, D4, B3, B4, S5, S6),
	make_unbound_vars(BY, BZ, S6, S7),
	emit(jump(L2), label(L1)),
	compile_body_expression(Z, NA, TAIL, D1, D5, B3, B5, S7, S8),
	make_unbound_vars(BZ, BY, S8, S2),
	emit(label(L2)),
	union(B4, B5, B2),
	both_determinate(D4, D5, D2).	
compile_body_expression((X -> Y; Z), NA, TAIL, D1, D2, B1, B2, S1, S2) :-
	gen_label(L1, S1, S3),
	gen_label(L2, S3, S4),
	emit(save_choice_points, push_choice_point(L1)),
	compile_body_expression(X, NA, nontail, D1, _, B1, B3, S4, S5),
	emit(restore_choice_points),
	collect_indexed_variables(Y, BY1), subtract(BY1, B3, BY),
	collect_indexed_variables(Z, BZ1), subtract(BZ1, B3, BZ),
	compile_body_expression(Y, NA, TAIL, D1, D4, B3, B4, S5, S6),
	make_unbound_vars(BY, BZ, S6, S7),
	emit(jump(L2), label(L1), restore_choice_points),
	compile_body_expression(Z, NA, TAIL, D1, D5, B3, B5, S7, S8),
	make_unbound_vars(BZ, BY, S8, S2),
	emit(label(L2)),
	union(B4, B5, B2),
	both_determinate(D4, D5, D2).

% disjunction
compile_body_expression((X; Y), NA, TAIL, D1, D2, B1, B2, S1, S2) :-
	gen_label(L1, S1, S3),
	gen_label(L2, S3, S4),
	emit(copy_choice_point(L1)),
	collect_indexed_variables(X, BX1), subtract(BX1, B1, BX),
	collect_indexed_variables(Y, BY1), subtract(BY1, B1, BY),
	compile_body_expression(X, NA, nontail, D1, D3, B1, B3, S4, S5),
	make_unbound_vars(BX, BY, S5, S6),
	emit(jump(L2), label(L1), no_redo, pop_choice_point),
	compile_body_expression(Y, NA, TAIL, D1, D4, B1, B4, S6, S7),
	make_unbound_vars(BY, BX, S7, S2),
	emit(label(L2)),
	union(B3, B4, B2),
	both_determinate(D3, D4, D2).

% cut
compile_body_expression(!, _, _, LAST/_, LAST2/det, B, B, S1, S2) :-
	( LAST == detlast
	-> LAST2 = detlast
	; LAST2 = last
	),
	gen_label(L, S1, S2),
	emit(cut(L)).

% true
compile_body_expression(true, _, _, D, D, B, B, S, S).

% fail
compile_body_expression(fail, _, _, D, D, B, B, S, S) :-
	emit(fail).

% repeat
compile_body_expression(repeat, _, _, _, notlast/nondet, B, B, S1, S2) :-
	gen_label(L, S1, S2),
	%% this clause can only be exited via cut, so just adjust ptrs in CP (no redo will ever happen)
	emit(adjust_choice_point(L), label(L)).
	
% not
compile_body_expression(\+X, NA, _, D, D, B1, B2, S1, S2) :-
	simple_test(X),
	% test is "simple"
	gen_label(L1, S1, S3),
	emit(simple_test(L1)),
	compile_body_expression(X, NA, nontail, D, _, B1, B2, S3, S2),
	emit(end_simple_test, fail, label(L1)).
compile_body_expression(\+X, NA, _, D, D, B1, B2, S1, S2) :-
	gen_label(L1, S1, S3),
	emit(save_choice_points, push_choice_point(L1)),
	compile_body_expression(X, NA, nontail, D, _, B1, B2, S3, S2),
	emit(restore_choice_points, fail, label(L1), restore_choice_points).

% once
compile_body_expression(once(X), NA, _, D, D, B1, B2, S1, S2) :-
	%% doesn't check test for being "simple" - that's ok
	gen_label(L1, S1, S3),
	gen_label(L2, S3, S4),
	emit(save_choice_points, push_choice_point(L1)),
	compile_body_expression(X, NA, nontail, D, _, B1, B2, S4, S2),
	emit(restore_choice_points, jump(L2)),
	emit(label(L1), restore_choice_points, fail, label(L2)).

% findall
compile_body_expression(findall(T, G, L), NA, TAIL, D, D, B1, B2, S1, S2) :-
	compile_body_expression('$findall_start', NA, nontail, D, _, B1, _, S1, S4),
	gensym('$findall_', P, S4, S5),
	goals_and_variables(G/T, VLIST, G2/T2, IARGS),
	map_second(VLIST, VARGS), % use real vars in head of newly created predicate
	HEAD =.. [P|VARGS],
	add_boilerplate(P, (HEAD :- G2, '$findall_push'(T2), fail)),
	HEAD2 =.. [P|IARGS],
	compile_body_expression(\+HEAD2, NA, nontail, D, _, B1, B3, S5, S6),
	!,
	compile_body_expression('$findall_collect'(L), NA, TAIL, D, _, B3, B2, S6, S2).

% forall
compile_body_expression(forall(G, A), NA, TAIL, D, D, B1, B2, S1, S2) :-
	gensym('$forall_', P, S1, S3),
	gensym('$forall_', P2, S3, S4),
	goals_and_variables(G/A, VLIST, G2/A2, IARGS),
	map_second(VLIST, VARGS),
	HEAD =.. [P|VARGS],
	add_boilerplate(P, (HEAD :- G2, \+(A2), !, fail)),
	add_boilerplate(P2, HEAD),
	HEAD2 =.. [P|IARGS],
	!,
	compile_body_expression(HEAD2, NA, TAIL, D, _, B1, B2, S4, S2).

% bagof
compile_body_expression(bagof(T, G, L), NA, TAIL, D1, D2, B1, B2, S1, S2) :-
	free_variables(G, T, [], VARS),
	drop_qualifiers(G, G2), 
	compile_bagof(T, NA, G2, L, VARS, TAIL, D1, D2, B1, B2, S1, S2).

% setof
compile_body_expression(setof(T, G, L), NA, TAIL, D1, D2, B1, B2, S1, S2) :-
	free_variables(G, T, [], VARS),
	drop_qualifiers(G, G2), 
	compile_setof(T, NA, G2, L, VARS, TAIL, D1, D2, B1, B2, S1, S2).

% catch
compile_body_expression(catch(G, B, R), NA, TAIL, D1, D2, B1, B2, S1, S2) :-
	gen_label(L1, S1, S3),
	gen_label(L2, S3, S4),
	collect_indexed_variables(B, BB1), subtract(BB1, B1, BB),
	%% create unbound vars in B, as they will otherwise be uninitialized
	%% if no throw occurred
	make_unbound_vars([], BB, S4, S5),
	emit(push_catcher(L1)),
	union(BB, B1, BB2),
	compile_body_expression(G, NA, nontail, D1, _, BB2, B3, S5, S6),
	emit(pop_catcher, jump(L2)), % G succeeds, no throw
	gensym('T', T, S6, S7),
	emit(label(L1), enter_catcher),  % throw occurred
	compile_term_for_unification(B, T, B3, B4, S7, S8),
	emit(unify_throw(T)), % ball unifies
	compile_body_expression(R, NA, TAIL, D1, D2, B4, B2, S8, S2), % recovery goal
	emit(label(L2)).

% if-then
compile_body_expression((X -> Y), NA, TAIL, D1, D2, B1, B2, S1, S2) :-
	compile_body_expression((X -> Y; fail), NA, TAIL, D1, D2, B1, B2, S1, S2).

% inline-unification
compile_body_expression(X = Y, _, _, D, D, B, B, S, S) :-
	%% same variable?
	indexed_variable(X, N),
	indexed_variable(Y, N).
compile_body_expression(X = Y, _, _, D, D, B1, B2, S1, S2) :-
	%% if certain to be cyclic, don't compile to simple assignment
	possibly_cyclic_unification(X = Y),
	message(['% explicit unification contains cycles: ', X = Y]),
	compile_explicit_unification(X, Y, unify, B1, B2, S1, S2).
compile_body_expression(X = Y, _, _, D, D, B1, B2, S1, S2) :-
	%% X is an unbound variable
	indexed_variable(X, N),
	\+member(N, B1), \+indexed_variable(Y, _),
	gensym('T', T, S1, S),
	compile_term_for_unification(Y, T, [N|B1], B2, S, S2),
	emit(assign(N, T)).
compile_body_expression(X = Y, _, _, D, D, B1, B2, S1, S2) :-
	%% Y is an unbound variable
	indexed_variable(Y, N),
	\+member(N, B1), \+indexed_variable(X, _),
	gensym('T', T, S1, S),
	compile_term_for_unification(X, T, [N|B1], B2, S, S2),
	emit(assign(N, T)).
compile_body_expression(X = Y, _, _, D, D, B1, B2, S1, S2) :-
	compile_explicit_unification(X, Y, unify, B1, B2, S1, S2).
compile_body_expression(X \= Y, _, _, D, D, B1, B2, S1, S2) :-
	compile_explicit_unification(X, Y, not_unify, B1, B2, S1, S2).

% identity comparison
compile_body_expression(X == Y, _, _, D, D, B1, B2, S1, S2) :-
	gensym('T', T1, S1, S3),
	compile_term_for_unification(X, T1, B1, B, S3, S4),
	gensym('T', T2, S4, S5),
	compile_term_for_unification(Y, T2, B, B2, S5, S2),
	emit(identical(T1, T2)).
compile_body_expression(X \== Y, _, _, D, D, B1, B2, S1, S2) :-
	gensym('T', T1, S1, S3),
	compile_term_for_unification(X, T1, B1, B, S3, S4),
	gensym('T', T2, S4, S5),
	compile_term_for_unification(Y, T2, B, B2, S5, S2),
	emit(not_identical(T1, T2)).

% arithmetic
compile_body_expression(X is EXP, _, _, D, D, B1, [N|B1], S1, S2) :-
	indexed_variable(X, N),
	\+member(N, B1),
	gensym('T', T1, S1, S),
	compile_arithmetic_expression(EXP, T1, B1, S, S2),
	emit(assign(N, T1)).
compile_body_expression(X is EXP, _, _, D, D, B1, B2, S1, S2) :-
	gensym('T', T1, S1, S3),
	compile_arithmetic_expression(EXP, T1, B1, S3, S4),
	gensym('T', T2, S4, S5),
	compile_term_for_unification(X, T2, B1, B2, S5, S2),
	emit(unify(T1, T2)).

compile_body_expression(X =:= Y, _, _, D, D, B, B, S1, S2) :-
	compile_arithmetic_test(X, Y, B, numerically_equal, S1, S2).
compile_body_expression(X =\= Y, _, _, D, D, B, B, S1, S2) :-
	compile_arithmetic_test(X, Y, B, numerically_not_equal, S1, S2).
compile_body_expression(X > Y, _, _, D, D, B, B, S1, S2) :-
	compile_arithmetic_test(X, Y, B, numerically_greater, S1, S2).
compile_body_expression(X < Y, _, _, D, D, B, B, S1, S2) :-
	compile_arithmetic_test(X, Y, B, numerically_less, S1, S2).
compile_body_expression(X >= Y, _, _, D, D, B, B, S1, S2) :-
	compile_arithmetic_test(X, Y, B, numerically_greater_or_equal, S1, S2).
compile_body_expression(X =< Y, _, _, D, D, B, B, S1, S2) :-
	compile_arithmetic_test(X, Y, B, numerically_less_or_equal, S1, S2).
			
% foreign call
compile_body_expression(foreign_call(CALL), _, _, D, D, B1, B2, S1, S2) :-
	CALL =.. [NAME|ARGS],
	compile_term_arguments(ARGS, [], DLIST, B1, B2, S1, S2),
	emit(foreign_call(NAME, DLIST)).

% global variable access
compile_body_expression(global_ref(NAME, RESULT), _, _, D, D, B1, B2, S1, S2) :-
	gensym('T', T1, S1, S3),
	gensym('T', T2, S3, S4),
	compile_term_for_unification(RESULT, T2, B1, B2, S4, S2),
	emit(global_ref(NAME, T1), unify(T1, T2)).
compile_body_expression(global_set(NAME, VALUE), _, _, D, D, B1, B2, S1, S2) :-
	gensym('T', T1, S1, S3),
	compile_term_for_unification(VALUE, T1, B1, B2, S3, S2),
	emit(global_set(NAME, T1)).

% suspend
compile_body_expression(suspend(X, Y), _, _, D, D, B1, B2, S1, S2) :-
	gensym('T', T1, S1, S3),
	gen_label(L, S3, S4),
	compile_term_for_unification(X, T1, B1, B3, S4, S5),
	emit(suspend(T1, L)),
	gensym('T', T2, S5, S6),
	compile_term_for_unification(Y, T2, B3, B2, S6, S2),
	emit(unify(T1, T2)).

% low-level call + address
compile_body_expression('$predicate_address'(N/A, PTR), NA, _, D, D, B1, B2, S1, S2) :-
	gensym('T', T1, S1, S3),
	gensym('T', T2, S3, S4),
	compile_term_for_unification(PTR, T2, B1, B2, S4, S2),
	register_call(NA, N/A),
	emit(predicate_address(N, A, T1), unify(T1, T2)).
compile_body_expression('$call'(PTR, ARGS), _, TAIL, LAST/D, LAST/nondet, B1, B2, S1, S2) :-
	gen_label(L, S1, S3),
	compile_term_arguments([PTR, ARGS], [], [R1, R2], B1, B2, S3, S2),
	compile_pointer_call(TAIL, LAST, D, R1, R2, L).

% delay
compile_body_expression(delay(V, G, P), NA, TAIL, D, D, B1, B2, S1, S2) :-
	compile_delayed_goal('$delay_goal', NA, V, G, P, TAIL, D, B1, B2, S1, S2).
compile_body_expression(delay(V, G), NA, TAIL, D, D, B1, B2, S1, S2) :-
	compile_delayed_goal('$delay_goal', NA, V, G, 1, TAIL, D, B1, B2, S1, S2).

% freeze
compile_body_expression(freeze(V, G), NA, TAIL, D, D, B1, B2, S1, S2) :-
	compile_delayed_goal('$freeze_goal', NA, V, G, 1, TAIL, D, B1, B2, S1, S2).

% type-, order- or ordinary predicate call
compile_body_expression(TERM, NA, TAIL, D1, D2, B1, B2, S1, S2) :-
	TERM =.. [NAME|ARGS],
	( compile_meta_predicate(NAME, NA, ARGS, TAIL, D1, D2, B1, B2, S1, S2)
	; compile_term_arguments(ARGS, [], DLIST, B1, B2, S1, S),
	  ( compile_type_predicate(NAME, DLIST), S2 = S, D2 = D1
	  ; DLIST = [X, Y], compile_order_predicate(NAME, X, Y), S2 = S, D2 = D1
	  ; compile_ordinary_call(NAME, NA, TAIL, DLIST, D1, D2, S, S2)
	  )
	).

% otherwise: error
compile_body_expression(TERM, _, _, _, _, _, _, _, _) :-
	error(['can not compile: ', TERM]).


%% inline unification
compile_explicit_unification(X, Y, OP, B1, B2, S1, S2) :-
	gensym('T', T1, S1, S3),
	compile_term_for_unification(X, T1, B1, B, S3, S4),
	gensym('T', T2, S4, S5),
	compile_term_for_unification(Y, T2, B, B2, S5, S2),
	INST =.. [OP, T1, T2],
	emit(INST).


%% compile delayed goal

compile_delayed_goal(INSTALLER, NA, V, G, PRIO, TAIL, D, B1, B2, S1, S2) :-
	( recorded(uses_delay, _)
	; recordz(uses_delay, yes),
	  message(['% delayed goal checks are enabled'])
	),
	gensym('$delayed_', P, S1, S3),
	gensym('$delay_', P2, S3, S4),
	goals_and_variables(G/V, VLIST, G2/V2, IARGS),
	map_second(VLIST, VARGS),
	HEAD =.. [P|VARGS],
	DHEAD =.. [P2|VARGS],
	length(VARGS, N),
	add_boilerplate(P, (HEAD :- G2)),
	INSTALL =.. [INSTALLER, V2, PRIO, PTR, VARGS],
	add_boilerplate(P2, (DHEAD :- '$predicate_address'(P/N, PTR), INSTALL)),
	DHEAD2 =.. [P2|IARGS],
	compile_body_expression(DHEAD2, NA, TAIL, D, _, B1, B2, S4, S2).


%% compile calls (meta-predicate-, ordinary or type-/order-predicate)

compile_meta_predicate(NAME, NA, ARGS, TAIL, D1, D2, B1, B2, S1, S2) :-
	length(ARGS, ARITY),
	is_meta_predicate(NAME, ARITY, SIG),
	compile_meta_arguments(ARGS, NAME/ARITY, SIG, [], DLIST, B1, B2, S1, S),
	compile_ordinary_call(NAME, NA, TAIL, DLIST, D1, D2, S, S2).
	
compile_ordinary_call(NAME, NA, TAIL, DLIST, LAST/D1, D2, S1, S2) :-
	length(DLIST, ARITY),
	register_unresolved_call(NAME/ARITY),
	( determinate_builtin(NAME, ARITY)
	-> D2 = LAST/D1
	%% self-tail-call and is last clause in set of determinate clauses?
	; ( TAIL/D1 == tail/det, NAME/ARITY == NA, LAST == detlast
	  -> D2 = last/det
	  ; D2 = LAST/nondet
	  )
	),
	gen_label(L, S1, S2),
	register_call(NA, NAME/ARITY),
	compile_call(TAIL, LAST, D1, NAME, DLIST, L).

compile_call(tail, _, det, NAME, DLIST, _) :- emit(tail_call(NAME, DLIST)).
compile_call(tail, last, _, NAME, DLIST, L) :- emit(final_call(NAME, DLIST, L)).
compile_call(tail, detlast, _, NAME, DLIST, L) :- emit(final_call(NAME, DLIST, L)).
compile_call(_, _, _, NAME, DLIST, L) :- emit(call(NAME, DLIST, L)).

compile_pointer_call(tail, _, det, R1, R2, _) :- emit(tail_call_address(R1, R2)).
compile_pointer_call(tail, last, _, R1, R2, L) :- emit(final_call_address(R1, R2, L)).
compile_pointer_call(tail, detlast, _, R1, R2, L) :- emit(final_call_address(R1, R2, L)).
compile_pointer_call(_, _, _, R1, R2, L) :- emit(call_address(R1, R2, L)).

compile_type_predicate(NAME, [VAL]) :-
	type_predicate(NAME),
	CALL =.. [NAME, VAL],
	emit(CALL).

compile_order_predicate('@<', X, Y) :- emit(term_less(X, Y)).
compile_order_predicate('@>', X, Y) :- emit(term_less(Y, X)).
compile_order_predicate('@>=', X, Y) :- emit(term_not_less(X, Y)).
compile_order_predicate('@=<', X, Y) :- emit(term_not_less(Y, X)).


%% helper predicates for arithmetic expressions

compile_arithmetic_test(X, Y, B, OP, S1, S2) :-
	compile_arithmetic_operation_arguments([X, Y], [T1, T2], B, S1, S2),
	TERM =.. [OP, T1, T2],
	emit(TERM).

compile_arithmetic_expression(+X, DEST, B, S1, S2) :-
	compile_arithmetic_expression(X, DEST, B, S1, S2).
compile_arithmetic_expression([X], DEST, _, S1, S2) :-
	number(X),
	register_literal(X, N, S1, S2),
	emit(literal(N, DEST, X)).
compile_arithmetic_expression(X, DEST, _, S1, S2) :-
	number(X),
	register_literal(X, N, S1, S2),
	emit(literal(N, DEST, X)).
compile_arithmetic_expression(X, DEST, B, S, S) :-
	indexed_variable(X, N),
	member(N, B),
	emit(local(N, DEST)).
compile_arithmetic_expression(X, _, _, _, _) :-
	indexed_variable(X, N),
	error(['unbound variable in arithmetic expression: ', N]). %XXX

compile_arithmetic_expression(EXP, DEST, B, S1, S2) :-
	functor(EXP, NAME, ARITY),
	EXP =.. [_|ARGS],
	arithmetic_operation(NAME, ARITY, OP),
	compile_arithmetic_operation_arguments(ARGS, DLIST, B, S1, S2),
	append(DLIST, [DEST], DLIST2),
	FN =.. [OP|DLIST2],
	emit(FN).

compile_arithmetic_expression(X, _, _, _, _) :-
	error(['invalid arithmetic expression: ', X]).

arithmetic_operation(abs, 1).
arithmetic_operation(atan, 1).
arithmetic_operation(ceiling, 1).
arithmetic_operation(cos, 1).
arithmetic_operation(exp, 1).
arithmetic_operation(float, 1).
arithmetic_operation(float_fractional_part, 1).
arithmetic_operation(float_integer_part, 1).
arithmetic_operation(floor, 1).
arithmetic_operation(log, 1).
arithmetic_operation(round, 1).
arithmetic_operation(sign, 1).
arithmetic_operation(sin, 1).
arithmetic_operation(tan, 1).
arithmetic_operation(sqrt, 1).
arithmetic_operation(truncate, 1).
arithmetic_operation(random, 1).
arithmetic_operation(clock, 0).
arithmetic_operation(xor, 2).
arithmetic_operation(rem, 2).
arithmetic_operation(max, 2).
arithmetic_operation(min, 2).

arithmetic_operation('+', 2, add).
arithmetic_operation('/\\', 2, bitwise_and).
arithmetic_operation('\\', 1, bitwise_not).
arithmetic_operation('<<', 2, shift_left).
arithmetic_operation('>>', 2, shift_right).
arithmetic_operation('\\/', 2, bitwise_or).
arithmetic_operation('**', 2, exponent).
arithmetic_operation('/', 2, divide).
arithmetic_operation('-', 1, negate).
arithmetic_operation('-', 2, subtract).
arithmetic_operation('//', 2, quotient).
arithmetic_operation('*', 2, multiply).
arithmetic_operation('\\\\', 2, rem).

arithmetic_operation(NAME, ARITY, NAME) :- arithmetic_operation(NAME, ARITY).

compile_arithmetic_operation_arguments([], [], _, S, S).
compile_arithmetic_operation_arguments([ARG], [T], B, S1, S2) :-
	gensym('T', T, S1, S),
	compile_arithmetic_expression(ARG, T, B, S, S2).
compile_arithmetic_operation_arguments([ARG1, ARG2], [T1, T2], B, S1, S2) :-
	compile_arithmetic_operation_arguments([ARG1], [T1], B, S1, S),
	compile_arithmetic_operation_arguments([ARG2], [T2], B, S, S2).

	
%% adding code to compiled-code-database

emit(T) :- recordz(code, T).
emit(T1, T2) :- emit(T1), emit(T2).
emit(T1, T2, T3) :- emit(T1, T2), emit(T3).
emit(T1, T2, T3, T4) :- emit(T1, T2, T3), emit(T4).
emit(T1, T2, T3, T4, T5) :- emit(T1, T2, T3, T4), emit(T5).


%% check if both determinate-flags are set

both_determinate(LAST/det, _/det, LAST/det).
both_determinate(LAST/_, _, LAST/nondet).


%% register literal data

register_literal(TERM, N, S, S) :-
	recorded(literal, [N|TERM]), !.
register_literal(TERM, N, S1, S2) :-
	gen_literal_index(N, S1, S2),
	recordz(literal, [N|TERM]).

register_literals([], [], S, S).
register_literals([L|R], [N|R2], S1, S) :-
	register_literal(L, N, S1, S2),
	register_literals(R, R2, S2, S).


%% create variables that are in the 2nd set but not in the first

make_unbound_vars(_, [], S, S).
make_unbound_vars(VS, [X|Y], S1, S) :-
	\+member(X, VS),
	!,
	gensym('T', T, S1, S2),
	emit(make_variable(T), assign(X, T)),
	make_unbound_vars(VS, Y, S2, S).
make_unbound_vars(VS, [_|Y], S1, S) :-
	make_unbound_vars(VS, Y, S1, S).


%% bagof/setof

compile_bagof(T, NA, G, L, [], TAIL, D1, D2, B1, B2, S1, S2) :-
	compile_body_expression((findall(T, G, L), L \== []), NA, TAIL, D1, D2, B1, B2, S1,
				S2).
compile_bagof(T, NA, G, L, VARS, TAIL, D1, D2, B1, B2, S1, S2) :-
	gensym('$bagof_', P, S1, S3),
	goals_and_variables(G/T, VLIST, G2/T2, IARGS),
	map_indexed_variables_to_real_variables(VARS, VLIST, VARS2),
	map_second(VLIST, VARGS),
	HEAD =.. [P|VARGS],
	add_boilerplate(P, (HEAD :- '$bagof_start'(VARS2, T2, T3), G2, '$findall_push'(T3),
			    fail)),
	HEAD2 =.. [P|IARGS],
	compile_body_expression(\+HEAD2, NA, nontail, D1, _, B1, B3, S3, S4),
	compile_body_expression('$bagof_finish'(L), NA, TAIL, D1, D2, B3, B2, S4, S2).

compile_setof(T, NA, G, L, [], TAIL, D1, D2, B1, B2, S1, S2) :-
	gensym('$setof_', P, S1, S3),
	goals_and_variables(G/T/L, VLIST, G2/T2/L2, IARGS),
	map_second(VLIST, VARGS),
	HEAD =.. [P|VARGS],
	add_boilerplate(P, (HEAD :- findall(T2, G2, TMP), TMP \== [], sort(TMP, L2))),
	HEAD2 =.. [P|IARGS],
	compile_body_expression(HEAD2, NA, TAIL, D1, D2, B1, B2, S3, S2).
compile_setof(T, NA, G, L, VARS, TAIL, D1, D2, B1, B2, S1, S2) :-
	gensym('$setof_', P, S1, S3),
	gensym('$setof_', P2, S3, S4),
	goals_and_variables(G/T/L, VLIST, G2/T2/L2, IARGS),
	map_indexed_variables_to_real_variables(VARS, VLIST, VARS2),
	map_second(VLIST, VARGS),
	HEAD =.. [P|VARGS],
	add_boilerplate(P, (HEAD :- '$bagof_start'(VARS2, T2, T3), G2, '$findall_push'(T3), fail)),
	add_boilerplate(P2, (HEAD :- '$bagof_finish'(TMP), sort(TMP, L2))),
	HEAD2 =.. [P|IARGS],
	compile_body_expression(HEAD2, NA, TAIL, D1, D2, B1, B2, S4, S2).


%% emit checks that '+'-moded arguments are instantiated

compile_mode_checks(N, A) :-
	get_head_modes(N, A, MODES),
	MODES \== none,
	emit_instantiation_checks(MODES, 0).
compile_mode_checks(_, _).

emit_instantiation_checks([], _).
emit_instantiation_checks(['+'|MODES], I) :-
	emit(check_nonvar(I)),
	I2 is I + 1,
	!,
	emit_instantiation_checks(MODES, I2).
emit_instantiation_checks([_|MODES], I) :-
	I2 is I + 1,
	emit_instantiation_checks(MODES, I2).
