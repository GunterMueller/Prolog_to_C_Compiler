%%%% generate cross-referencing information


emit_xref_information :-
	dump_directives,
	dump_defined_predicates,
	%dump_unknown_predicates,
	dump_determinate_predicates,
	dump_call_information.

dump_directives :-
	forall(recorded(directive, DECL), write_xref(directive(DECL))).

dump_defined_predicates :-
	forall(recorded(defined, N/A), write_xref(defined(N/A))).

dump_unknown_predicates :-
	forall((recorded(unresolved, N/A),
		\+auto_include(N, A, _)),
	       write_xref(unknown(N/A))).

dump_call_information :-
	forall(recorded(calls, calls(NA1, NA2)), dump_call_information(NA1, NA2)).

dump_call_information(N1/A1, N2/A2) :-
	recorded(defined, N1/A1),
	write_xref(calls(N1/A1, N2/A2)).
dump_call_information(_, _).

dump_determinate_predicates :-
	forall((recorded(determinate_predicate, N/A),
		recorded(defined, N/A)),
	       write_xref(determinate(N/A))).

%% ignore operators, and quote args, also add ".\n"
write_xref(FORM) :-
	FORM =.. [NAME|ARGS],
	writeq(NAME),
	write_xref_args(ARGS),
	!.

write_xref_args([]).
write_xref_args([X|R]) :-
	put_char('('),
	writeq(X),
	forall(member(Y, R), (put_char(','), writeq(Y))),
	display(').\n').


%% register defined or unresolved predicates

register_unresolved_call(NA) :-
	(recorded(defined, NA)
	; recorded(unresolved, NA)
	; recordz(unresolved, NA)
	).

register_defined_predicate(NA) :-
	(recorded(defined, NA)
	-> N/A = NA, error(['Non-contiguous predicate definition: ', N, '/', A])
	; recordz(defined, NA)
	),
	recorded(unresolved, NA, REF), erase(REF).
register_defined_predicate(_).


%% operations on the call-tree

%% FROM, TO: NAME/ARITY
register_call(FROM, FROM).
register_call(FROM, TO) :-
	recordz(calls, calls(FROM, TO)).

predicate_callers(NA, CALLERS) :-
	findall(CALLER, recorded(calls, calls(CALLER, NA)), CALLERS).


%% register whether compiled predicate is determinate

mark_as_determinate(N/A, det) :-
	\+determinate_builtin(N, A),
	recordz(determinate_predicate, N/A).
mark_as_determinate(_, _).
