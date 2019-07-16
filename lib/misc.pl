%%% miscellaneous predicates


:- determinate '$univ_args'/4.


compare(>, X, Y) :- X @> Y, !.
compare(<, X, Y) :- X @< Y, !.
compare(=, X, X).

throw(BALL) :-
	copy_term(BALL, B2),
	foreign_call(do_throw(B2)).

shell(CMD) :- shell(CMD, 0).

name(X, S) :-
	var(X), !,
	(number_codes(X, S); atom_codes(X, S)).
name(X, S) :-
	number(X), !, number_codes(X, S).
name(X, S) :- atom_codes(X, S).


X =.. Y :- atomic(X), !, Y = [X].
X =.. Y :-
	compound(X), !,
	functor(X, NAME, ARITY),
	Y = [NAME|ARGS],	
	'$univ_args'(X, 1, ARITY, ARGS).
X =.. [N|ARGS] :-
	var(X),
	length(ARGS, ARITY),
	foreign_call(do_make_term(ARITY, N, ARGS, X)).

'$univ_args'(TERM, I, N, []) :- I > N, !.
'$univ_args'(TERM, I, N, [X|MORE]) :-
	I =< N,
	arg(I, TERM, X),
	I2 is I + 1,
	'$univ_args'(TERM, I2, N, MORE).

deref_term(X, L, D, Y) :-
	(foreign_call(deref_term(X, L, D, Y1))
	; garbage_collect, deref_term(X, L, D, Y1)),
	!, Y = Y1.

%%XXX these will loop if heap remains insufficient
copy_term(X, Y) :- var(X), !, deref_term(Y, 999999, 0, X).
copy_term(X, Y) :- deref_term(X, 999999, 0, Y). % failed: forces GC

duplicate_term(X, Y) :- var(X), !, deref_term(Y, 999999, 1, X).
duplicate_term(X, Y) :- deref_term(X, 999999, 1, Y).

%% once again, from R. O'Keefe
between(L, U, X) :-
    (   integer(L), integer(U) ->
        (   integer(X) ->
            L =< X, X =< U
        ;   var(X) ->
            L =< U,
            '$between'(L, U, X)
        ;   throw(type_error(integer, X))
        )
    ;   integer(L), integer(X), L > X -> fail
    ;   integer(U), integer(X), X > U -> fail
    ;   throw(error('between/3: invalid arguments'))
    ).

'$between'(L, L, L) :- !.
'$between'(L, _, L).
'$between'(L, U, X) :-
    M is L + 1,
    '$between'(M, U, X).

unify_with_occurs_check(X, X) :- acyclic_term(X).

atom_codes(A, LST) :-
	var(A), !, foreign_call(atom_codes(A, LST)).
atom_codes(A, LST) :-
	foreign_call(atom_codes(A, LST1)),
	(LST1 == 0 -> atom_codes(A, LST); LST = LST1).

number_codes(A, LST) :-
	var(A), !, foreign_call(number_codes(A, LST)).
number_codes(A, LST) :-
	foreign_call(number_codes(A, LST1)),
	(LST1 == 0 -> number_codes(A, LST); LST = LST1).

atom_number(A, N) :-
	var(A),
	!,
	foreign_call(num_to_atom(N, A)).
atom_number(A, N) :-
	foreign_call(atom_to_num(A, N)).
