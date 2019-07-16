%%%% Prolog-preprocessor (provides "main" entry-point)
%
% - does not process user-defined meta-predicate arguments.


:- ensure_loaded(library(dcg)).
:- discontiguous term_expansion/2, goal_expansion/2.


%% so at least one clause is defined
term_expansion(_, _) :- fail.

goal_expansion(_, _) :- fail.


expand_term(X, Y) :-
	term_expansion(X, Y1),
	!,
	( X == Y1
	-> Y = X
	; ( is_list(Y1)
	  -> expand_term_list(Y1, Y)
	  ; expand_term(Y1, Y)
	  )
	).
expand_term(X, X).

expand_term_list([], []).
expand_term_list([X|R1], [Y|R2]) :-
	expand_term(X, Y),
	expand_term_list(R1, R2).

expand_goal(X, Y) :-
	goal_expansion(X, Y1),
	!,
	(X == Y1 -> Y = X; expand_goal(Y1, Y)).
expand_goal(X, Y) :-
	functor(X, NAME, ARITY),
	X =.. [_|ARGS],
	( pp_meta_predicate(NAME, ARITY, SIG)
	; recorded(pp_meta_predicate_signature, m(NAME, ARITY, SIG))
	),
	expand_meta_goal(ARGS, SIG, XARGS),
	Y =.. [NAME|XARGS].
expand_goal(X, X).

expand_meta_goal([], _, []).
expand_meta_goal([A|AR], [], [A|AR2]) :-
	expand_meta_goal(AR, [], AR2).
expand_meta_goal([A|AR], [0|SR], [XA|AR2]) :-
	expand_goal(A, XA),
	expand_meta_goal(AR, SR, AR2).
expand_meta_goal([A|AR], [_|SR], [A|AR2]) :-
	expand_goal(A, XA),
	expand_meta_goal(AR, SR, AR2).

pp_meta_predicate(catch, 3, [0, +, 0]).
pp_meta_predicate(findall, 3, [+, 0, +]).
pp_meta_predicate(setof, 3, [+, 0, +]).
pp_meta_predicate(bagof, 3, [+, 0, +]).
pp_meta_predicate(delay, 2, [+, 0]).
pp_meta_predicate(freeze, 2, [+, 0]).
pp_meta_predicate(',', 2, [0, 0]).
pp_meta_predicate(';', 2, [0, 0]).
pp_meta_predicate('->', 2, [0, 0]).
pp_meta_predicate(forall, 2, [0, 0]).
pp_meta_predicate(once, 1, [0]).
pp_meta_predicate('\\+', 1, [0]).

expand_goal_list([], []).
expand_goal_list([X|R], [Y|R2]) :-
	expand_goal(X, Y),
	!, expand_goal_list(R, R2).


%% entry-point - reads current_input and writes expansions to current_output

main :-
	read(TERM),
	TERM \== end_of_file,
	process_term(TERM),
	!, main.
main.

process_term(TERM) :-
	expand_term(TERM, XTERM),
	( is_list(XTERM)
	-> forall(member(XT, XTERM), process_expanded_term(XT))
	; process_expanded_term(XTERM)
	).

process_expanded_term((HEAD --> BODY)) :-
        dcg_rule((HEAD --> BODY), X1),
	process_expanded_term(X1).
process_expanded_term((X :- Y)) :-
        expand_goal(Y, Y1),
	output_expanded_term((X :- Y1)).
process_expanded_term((:- DECL)) :-
        process_expanded_directive(DECL),
	output_expanded_term((X :- Y1)).
process_expanded_term(TERM) :-
        output_expanded_term(TERM).

output_expanded_term(TERM) :-
	writeq(TERM), display('.\n\n').

process_expanded_directive((X, Y)) :-
	process_expanded_directive(X),
	process_expanded_directive(Y).
process_expanded_directive(op(A, B, C)) :-
	op(A, B, C).
process_expanded_directive(meta_predicate(PI)) :-
	process_expanded_meta_predicate(PI).

process_expanded_meta_predicate((X, Y)) :-
	process_expanded_meta_predicate(X),
	process_expanded_meta_predicate(Y).
process_expanded_meta_predicate(X) :-
	functor(X, NAME, ARITY),
	X =.. [_|SIG],
	recordz(pp_meta_predicate_signature, m(NAME, ARITY, SIG)).
register_meta_predicate(X) :-
	throw(syntax_error('invalid meta_predicate declaration')).
