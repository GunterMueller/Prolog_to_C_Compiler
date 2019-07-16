%%% clause indexing


%% build map from clause-indices to types (clauses must have arity > 0)

build_index_to_type_map([], _, []).
build_index_to_type_map([(HEAD :- _)|MORE], I, [I/T1|ENTRIES]) :-
        !,
	(HEAD =.. [_, A1|_] -> get_argument_type(A1, T1); T1 = none),
	I2 is I + 1,
	build_index_to_type_map(MORE, I2, ENTRIES).
build_index_to_type_map([HEAD|MORE], I, ENTRIES) :-	
	build_index_to_type_map([(HEAD :- true)|MORE], I, ENTRIES).

get_argument_type(X, var) :- var(X), !.
get_argument_type([], null).
get_argument_type([_|_], pair).
get_argument_type(X, integer(X)) :- integer(X), !.
get_argument_type(X, float) :- number(X), !.
get_argument_type(X, atom(X)) :- atom(X), !.
get_argument_type(X, structure(N/A)) :- compound(X), !, functor(X, N, A).


%% compile indexing instructions

compile_clause_indexing(_, _, [_/none|_], S, S). % arity 0 - nothing to do
compile_clause_indexing(_, _, [_/var|_], S, S). % first case is var
compile_clause_indexing(N, A, MAP, S1, S2) :-
	scan_indexing_types(MAP, [], DMAP),
	compile_dispatch(DMAP, N, A, S1, S2).


%% collect list of types to dispatch on

scan_indexing_types([], _, []).
scan_indexing_types([I/var|_], _, [I/var]). % variable will match anything
scan_indexing_types([_/T|MORE], DONE, R) :- % type already seen
	member_in_indexing_types(T, DONE),
	scan_indexing_types(MORE, DONE, R).
scan_indexing_types([I/T|MORE], DONE, [I/T|R]) :-
	scan_indexing_types(MORE, [T|DONE], R).

member_in_indexing_types(_, []) :- !, fail.
member_in_indexing_types(T, [T|_]) :- !.
member_in_indexing_types(T, [_|MORE]) :- member_in_indexing_types(T, MORE).
			 

%% compile code to dispatch on type

compile_dispatch(DMAP, N, A, S1, S2) :-
	%% remaining clauses contain integer cases
	findall(X, (member(X, DMAP), X = _/integer(_)), ICASES),
	ICASES \== [],
	!,
	subtract(DMAP, ICASES, DMAP2),
	DMAP = [I2/_|_],
	secondary_clause_label(N, A, I2, L2),
	%% test for fixnum first, otherwise run first clause if arg is var
	gen_label(L1, S1, S3),	% no integer case matches
	gen_label(L3, S3, S4),
	emit(switch_on_integer(L1), switch_on_var(L2), jump(L3), label(L1)),
	findall(NUM/LABEL, (member(I/integer(NUM), ICASES),
			    secondary_clause_label(N, A, I, LABEL)),
		TABLE),
	emit(dispatch_on_integer(TABLE)),
	%% no integer case matches - dispatch on non-integer cases
	compile_dispatch_integer_fail(DMAP2, N, A, FL, S4, S5),
	emit(label(L3)),
	compile_dispatch_sequence(DMAP2, N, A, s(FL), S5, S2).
compile_dispatch(DMAP, N, A, S1, S2) :-
	%% remaining clauses contain no integer cases
	memberchk(I1/var, DMAP),
	!,
	DMAP = [I2/_|_],
	secondary_clause_label(N, A, I1, L1),
	secondary_clause_label(N, A, I2, L2),
	%% test for fixnum case and invoke var-case, otherwise run first clause if arg is a var
	emit(switch_on_integer(L1), switch_on_var(L2)),
	compile_dispatch_sequence(DMAP, N, A, s(no), S1, S2).
compile_dispatch(DMAP, N, A, S1, S2) :-
	%% no integer or var case
	gen_label(FL, S1, S3),
	gen_label(L1, S3, S4),
	DMAP = [I/_|_],
	secondary_clause_label(N, A, I, L2),
	%% integer fails, var runs first clause, and create fail-block
	emit(switch_on_integer(FL), switch_on_var(L2), jump(L1)),
	emit(label(FL), no_redo, fail, label(L1)),
	compile_dispatch_sequence(DMAP, N, A, s(FL), S4, S2).

compile_dispatch_integer_fail(DMAP, N, A, no, S, S) :-
	%% arg was non-matching integer - is there a var case?
	memberchk(I1/var, DMAP),
	!,
	secondary_clause_label(N, A, I1, L1),
	emit(jump(L1)).
compile_dispatch_integer_fail(_, _, _, FL, S1, S2) :-
	%% generate fail block
	gen_label(FL, S1, S2),
	emit(label(FL), no_redo, fail).

compile_dispatch_sequence([], _, _, s(no), S, S) :-
	emit(no_redo, fail).
compile_dispatch_sequence([], _, _, s(FL), S, S) :- % re-use fail-point
	emit(jump(FL)).
compile_dispatch_sequence([I/var], N, A, _, S, S) :-
	secondary_clause_label(N, A, I, L),
	emit(jump(L)).
compile_dispatch_sequence([I1/atom(ATM1)|DMAP], N, A, XS, S1, S2) :-
	build_dispatch_table(N/A, atom, atom_table_index_threshold, [I1/atom(ATM1)|DMAP],
			     ENTRIES3, TLEN, DMAP2),
	findall(KEY-ATOM/LABEL, (member(KEY-ATOM/INDEX, ENTRIES3),
				 secondary_clause_label(N, A, INDEX, LABEL) ),
		ENTRIES4),
	gen_label(LX, S1, S3),
	emit(switch_and_dispatch_on_atom(ENTRIES4, TLEN, LX)),
	compile_dispatch_sequence(DMAP2, N, A, XS, S3, S2).
compile_dispatch_sequence([I1/structure(NA1)|DMAP], N, A, XS, S1, S2) :-
	build_dispatch_table(N/A, structure, structure_table_index_threshold,
			     [I1/structure(NA1)|DMAP], ENTRIES3, TLEN, DMAP2),
	findall(KEY-NA/LABEL, (member(KEY-NA/INDEX, ENTRIES3),
			       secondary_clause_label(N, A, INDEX, LABEL) ),
		ENTRIES4),
	gen_label(LX, S1, S3),
	emit(switch_and_dispatch_on_structure(ENTRIES4, TLEN, LX)),
	compile_dispatch_sequence(DMAP2, N, A, XS, S3, S2).
compile_dispatch_sequence([I/T|DMAP], N, A, XS, S1, S2) :-
	dispatch_instruction(T, INSTNAME),
	secondary_clause_label(N, A, I, L),
	INST =.. [INSTNAME, L],
	emit(INST),
 	compile_dispatch_sequence(DMAP, N, A, XS, S1, S2).
	

%% integer already handled
dispatch_instruction(var, switch_on_var).
dispatch_instruction(null, switch_on_null).
dispatch_instruction(pair, switch_on_pair).
dispatch_instruction(float, switch_on_float).
dispatch_instruction(atom(_), switch_on_atom).
dispatch_instruction(structure(_), switch_on_structure).


%% adjust atom dispatch-table by moving colliding entries

adjust_dispatch_table(E1, LEN, E, COUNT) :-
	duplicate_dispatch_table_entries(E1, ED, E2),
	insert_dispatch_table_entries(ED, LEN, E2, E3),
	length(ED, COUNT),
	keysort(E3, E).

duplicate_dispatch_table_entries([], [], []).
duplicate_dispatch_table_entries([I1-E1, I1-E2|ER], [I1-E2|ER2], EN) :-
	duplicate_dispatch_table_entries([I1-E1|ER], ER2, EN).
duplicate_dispatch_table_entries([X|ER], ER2, [X|EN]) :-
	duplicate_dispatch_table_entries(ER, ER2, EN).

insert_dispatch_table_entries([], _, E, E).
insert_dispatch_table_entries([I1-E1|ER], LEN, ES, ER2) :-
	I2 is I1 + 1,
	find_free_dispatch_table_entry(I2, LEN, ES, II),
	insert_dispatch_table_entries(ER, LEN, [II-E1|ES], ER2).

find_free_dispatch_table_entry(I, LEN, ES, IR) :-
	I >= LEN,
	find_free_dispatch_table_entry(0, LEN, ES, IR).
find_free_dispatch_table_entry(I, LEN, ES, IR) :-
	member(I-_, ES),
	!,
	I2 is I + 1,
	find_free_dispatch_table_entry(I2, LEN, ES, IR).
find_free_dispatch_table_entry(I, _, _, I).


%% create table of first clause-indices in set of clauses, with one
%% entry per type, from type-map (used in first-level claus-indexing
%% for fact-blocks)
%
% TABLE = [INTEGER, ATOM, NULL, PAIR, STRUCTURE]

type_map_first_indices(MAP, TABLE) :-
	type_map_first_indices(MAP, 0, 0, 0, 0, 0, TABLE).

type_map_first_indices([], I, A, N, P, S, [I, A, N, P, S]).
type_map_first_indices([C/integer(_)|R], 0, A, N, P, S, TABLE) :-
	type_map_first_indices(R, C, A, N, P, S, TABLE).
type_map_first_indices([C/atom(_)|R], I, 0, N, P, S, TABLE) :-
	type_map_first_indices(R, I, C, N, P, S, TABLE).
type_map_first_indices([C/null|R], I, A, 0, P, S, TABLE) :-
	type_map_first_indices(R, I, A, C, P, S, TABLE).
type_map_first_indices([C/pair|R], I, A, N, 0, S, TABLE) :-
	type_map_first_indices(R, I, A, N, C, S, TABLE).
type_map_first_indices([C/structure(_)|R], I, A, N, P, 0, TABLE) :-
	type_map_first_indices(R, I, A, N, P, C, TABLE).
type_map_first_indices([_|R], I, A, N, P, S, TABLE) :-
	type_map_first_indices(R, I, A, N, P, S, TABLE).


%% build hash-table from elements in dispatch map, mapping
%  element-hashes to clause-indices (used for fact-blocks)

build_dispatch_table(NA, DFUNCTOR, THRESHOLD, DMAP, DTABLE, TLEN, DMAP2) :-
	findall(X, (member(X, DMAP), X = _/DMATCH, functor(DMATCH, DFUNCTOR, 1)), CASES),
	length(CASES, TL),
	default_setting(THRESHOLD, T), TL >= T,
	%% match atom(X), integer(X), structure(X)
	findall(ITEM/LABEL, (member(LABEL/CASE, CASES), arg(1, CASE, ITEM)), TABLE),
	length(TABLE, LEN),
	default_setting(dispatch_table_size_factor, F),
	TLEN is F * LEN,
	findall(KEY-(ITEM/LABEL),
		(member(ITEM/LABEL, TABLE),
		 build_dispatch_table_entry(ITEM, HASH),
		 KEY is HASH rem TLEN),
		ENTRIES),
	keysort(ENTRIES, ENTRIES2),
	adjust_dispatch_table(ENTRIES2, TLEN, DTABLE, DUPS),
	message(['% collisions in ', DFUNCTOR, ' dispatch table for ', NA, ': ', DUPS/TLEN]),
	subtract(DMAP, CASES, DMAP2).

build_dispatch_table_entry(NAME/ARITY, HASH) :-
	!, atom_hash(NAME, HASH1), HASH is HASH1 + ARITY.
build_dispatch_table_entry(INT, INT) :-
	integer(INT), !.
build_dispatch_table_entry(ATOM, HASH) :-
	atom_hash(ATOM, HASH).
