%%% ISO-compliant read_term/2+3


:- mode '$read_term'(+, +, -, -),
	'$collect_vars'(+, -).


read_term(TERM, OPTS) :-
	'$read_term'(OPTS, current_input, TERM, _).

read_term(IN, TERM, OPTS) :-
	'$read_term'(OPTS, IN, TERM, _).

'$read_term'([], IN, TERM, VARS) :-
	!,
	foreign_call(current_input_stream(OLD)),
	foreign_call(set_current_input_stream(IN)),
	'$read1'(TERM, VARS),
	foreign_call(set_current_input_stream(OLD)).
'$read_term'([variable_names(VARS)|MORE], IN, TERM, VARS1) :-
	!,
	'$read_term'(MORE, IN, TERM, VARS1),
	VARS = VARS1.
'$read_term'([variables(VARS)|MORE], IN, TERM, VARS1) :-
	!,
	'$read_term'(MORE, IN, TERM, VARS1),
	'$collect_vars'(VARS1, VARS).
'$read_term'([OPT|_], _, _, _) :-
	throw(domain_error(read_option, OPT)).

'$collect_vars'([], []) :- !.
'$collect_vars'([X|MORE], [V|R]) :-
	arg(2, X, V),
	'$collect_vars'(MORE, R).
