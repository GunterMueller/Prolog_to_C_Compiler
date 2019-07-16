%%%% interpreter toplevel


:- include('lib/interp.pl').
:- include('lib/dcg.pl').

main :-
	get_library_dir(DIR),
	pi_init(DIR),
	command_line_arguments(ARGS),
	'$predicate_address'(dcg_rule/2, ADR),
	asserta((term_expansion((X --> Y), Z) :- '$call'(ADR, [(X --> Y), Z]))),
	parse_arguments(ARGS),
	!,
	((recorded(pi_initialization_goal, G); recorded(pi_default_initialization_goal, G))
	 -> call(G)
	 ; recorded(pi_silent, _, REF), erase(REF), repl).

get_library_dir(DIR) :-
	(getenv('PC_LIBRARY_DIR', DIR); DIR = 'lib').

repl :-
	display('?- '), flush_output,
	seeing(IN), telling(OUT),
	catch('$read1'(TERM, VARS), EXN, (report_exception(EXN), !, repl)),
	(TERM == end_of_file, halt; process_input(TERM, VARS, IN, OUT)).
repl :- repl.

%% always fails, to ensure trail is unwound
process_input(TERM, _, _, _) :-
	compound(TERM),	TERM = [_|_], 
	catch(consult_files(TERM), EXN, report_exception(EXN)),
	see(IN), tell(OUT),
	!, fail.
process_input(TERM, VARS, IN, OUT) :-
	catch(run_goal(TERM, VARS), EXN, report_exception(EXN)),
	see(IN), tell(OUT),
	!, fail.
process_input(TERM, _, IN, OUT) :-
	see(IN), tell(OUT),
	display('no.\n'), !, fail.

consult_files([]).
consult_files([F|R]) :- consult(F), consult_files(R).

run_goal(G, VARS) :-
	call(G),
	show_variables(VARS),
	display(' ? '), flush_output,
	(get_response -> fail; display('\nyes.\n')).

get_response :-
	get0(59), skip_line.

skip_line :-
	repeat, get0(10), !.

show_variables([]) :- !.
show_variables([NAME=X|MORE]) :-
	nl, display(NAME), display(' = '), writeq(X),
	show_variables(MORE).

report_exception(EXN) :-
	display('\nUncaught exception:\n'),
	writeq(EXN), nl.

parse_arguments([]).
parse_arguments(['-h'|_]) :- usage(0).
parse_arguments(['-help'|_]) :- usage(0).
parse_arguments(['--help'|_]) :- usage(0).
parse_arguments(['-version'|_]) :- show_version_and_exit.
parse_arguments(['-n'|MORE]) :-
	recorded(pi_library_dir, _, REF), erase(REF),
	recordz(pi_library_dir, 'lib'),
	parse_arguments(MORE).
parse_arguments(['-t'|MORE]) :-
	global_set(pi_trace_depth, 0),
	parse_arguments(MORE).
parse_arguments(['-q'|MORE]) :-
	recordz(pi_silent, yes),
	parse_arguments(MORE).
parse_arguments(['-i', G|MORE]) :-
	recordz(pi_default_initialization_goal, G),
	parse_arguments(MORE).
parse_arguments([FILENAME|_]) :-
	name(FILENAME, [45|_]), usage(1).
parse_arguments([FILENAME|MORE]) :-
	consult(FILENAME),
	parse_arguments(MORE).

usage(CODE) :-
	display('usage: pi [-version] [-n] [-q] [-h] [-t] [-i NAME] FILENAME ...\n'),
	halt(CODE).

show_version_and_exit :-
	current_prolog_flag(version, V),
	current_prolog_flag(prolog_title, T),
	current_prolog_flag(prolog_copyright, C),
	writef("%d version %d - %d\n", [T, V, C]),
	halt.
