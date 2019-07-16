%%% operations on terms


%% gather list of variables, assigning indices by unifying each var with '_var_'(INDEX),
%% also returns list of variable-indices that are referenced multiple times

index_variables(X, VARS, REFD) :-
	index_variables(X, 0, _, VARS, REFD).
index_variables(X, C1, C2, [X], []) :-
	var(X), !, indexed_variable(X, C1), C2 is C1 + 1.
index_variables(X, C, C, [], [N]) :-
	indexed_variable(X, N), !.
index_variables(X, C, C, [], []) :- atomic(X), !.
index_variables([X|R], C1, C2, L, REFD) :-
	index_variables(X, C1, Cx, L1, REFD1),
	index_variables(R, Cx, C2, L2, REFD2),
	append(L1, L2, L),
	union(REFD1, REFD2, REFD),
	!.
index_variables(T, C1, C2, V, REFD) :-
	T =.. L, index_variables(L, C1, C2, V, REFD).


%% wrapping and unwrapping of indexed variables - we must take care not to
%% have direct occurrences of '_var_'(N), or they may be compiled incorrectly

indexed_variable(X, I) :- X =.. ['_var_', I].


%% check if term contains variables and fail if it does

ground_term(X) :- indexed_variable(X, _), !, fail.
ground_term([X|Y]) :-
	!, ground_term(X), ground_term(Y).
ground_term(X) :- atomic(X), !.
ground_term(X) :-
	X =.. LST,
	ground_term(LST).


%% collect instances of '_var_'(N) into lists of indexes

collect_indexed_variables(X, [N]) :-
	indexed_variable(X, N), !.
collect_indexed_variables(X, []) :-
	atomic(X), !.
collect_indexed_variables(X, NS) :-
	X =.. [_|ARGS],
	collect_indexed_variables(ARGS, [], NS).

collect_indexed_variables([], NS, NS).
collect_indexed_variables([X|Y], NS1, NS) :-
	collect_indexed_variables(X, NS2),
	union(NS1, NS2, NS3),
	collect_indexed_variables(Y, NS3, NS).


%% using a map of variable-indices and real variables, build a new term with instances
%% of '_var_'(N) replaced by the real variables

map_indexed_variables_to_real_variables(X, VLIST, V) :-
	indexed_variable(X, I),
	!,
	member(I/V, VLIST).
map_indexed_variables_to_real_variables(X, _, X) :-
	atomic(X),
	!.
map_indexed_variables_to_real_variables([X|Y], VLIST, [Z|U]) :-
	!,
	map_indexed_variables_to_real_variables(X, VLIST, Z),
	map_indexed_variables_to_real_variables(Y, VLIST, U).
map_indexed_variables_to_real_variables(X, VLIST, Y) :-
	X =.. [N|ARGS],
	map_indexed_variables_to_real_variables(ARGS, VLIST, ARGS2),
	Y =.. [N|ARGS2].


%% support for bagof/setof

% (taken from setof.pl of the DEC10 library)

%   In order to handle variables properly, we have to find all the 
%   universally quantified variables in the Generator.  All variables
%   as yet unbound are universally quantified, unless
%	a)  they occur in the template
%	b)  they are bound by X^P, setof, or bagof
%   free_variables(Generator, Template, OldList, NewList)
%   finds this set, using OldList as an accumulator.

free_variables(Term, Bound, VarList, [Term|VarList]) :-
	indexed_variable(Term, I),
	term_is_free_of(Bound, I),
	list_is_free_of(VarList, I),
	!.
free_variables(Term, _, VarList, VarList) :-
	indexed_variable(Term, _),
	!.
free_variables(Term, Bound, OldList, NewList) :-
	explicit_binding(Term, Bound, NewTerm, NewBound),
	!,
	free_variables(NewTerm, NewBound, OldList, NewList).
free_variables(Term, Bound, OldList, NewList) :-
	functor(Term, _, N),
	free_variables(N, Term, Bound, OldList, NewList).

free_variables(0, _, _, VarList, VarList) :- !.
free_variables(N, Term, Bound, OldList, NewList) :-
	arg(N, Term, Argument),
	free_variables(Argument, Bound, OldList, MidList),
	M is N-1, !,
	free_variables(M, Term, Bound, MidList, NewList).


%   explicit_binding checks for goals known to existentially quantify
%   one or more variables.  In particular \+ is quite common.

explicit_binding(\+ _,  	       Bound, fail,	Bound      ) :- !.
explicit_binding(not(_),	       Bound, fail,	Bound	   ) :- !.
explicit_binding(Var^Goal,	       Bound, Goal,	Bound+Var) :- !.
explicit_binding(setof(Var,Goal,Set),  Bound, Goal-Set, Bound+Var) :- !.
explicit_binding(bagof(Var,Goal,Bag),  Bound, Goal-Bag, Bound+Var) :- !.

term_is_free_of(Term, I) :-
	indexed_variable(Term, I2), !,
	I2 =\= I.
term_is_free_of(Term, I) :-
	functor(Term, _, N),
	term_is_free_of(N, Term, I).

term_is_free_of(0, _, _) :- !.
term_is_free_of(N, Term, I) :-
	arg(N, Term, Argument),
	term_is_free_of(Argument, I),
	M is N-1, !,
	term_is_free_of(M, Term, I).

list_is_free_of([Head|Tail], I) :-
	indexed_variable(Head, I2),
	I2 =\= I,
	!,
	list_is_free_of(Tail, I).
list_is_free_of([_|R], I) :-
	list_is_free_of(R, I).
list_is_free_of([], _).


%% drop existential qualifiers from expression

drop_qualifiers(_^X, Y) :- !, drop_qualifiers(X, Y).
drop_qualifiers(X, X).


%% collect variables into index/realvar lists and produce new expressions containing the 
%% latter, together with a new list of indexed vars usable is environment in a new clause

goals_and_variables(GOAL, VLIST, NEWGOAL, IARGS) :-
	collect_indexed_variables(GOAL, GVARS),
	findall(I/_, member(I, GVARS), VLIST),
	map_indexed_variables_to_real_variables(GOAL, VLIST, NEWGOAL),
	findall(V, (member(I/_, VLIST), indexed_variable(V, I)), IARGS).


%% check term of the form VAR = TERM or TERM = VAR for being cyclic

possibly_cyclic_unification(X = Y) :-
	collect_indexed_variables(X, XV),
	collect_indexed_variables(Y, YV),
	intersection(XV, YV, [_|_]),
	(indexed_variable(X, _); indexed_variable(Y, _)).


%% canonicalize predicate-indicator

canonical_pi(NAME/ARITY, NAME/ARITY) :-
	atom(NAME), integer(ARITY), ARITY >= 0.
canonical_pi(NAME, NAME/_) :-
	atom(NAME).


%% combine list of terms into comma-separated goal

combine_comma_separated_goals([], true).
combine_comma_separated_goals([G], G).
combine_comma_separated_goals([G|R], (G, G2)) :-
	combine_comma_separated_goals(R, G2).


%% extract functor from general clause

clause_functor((HEAD :- _), N, A) :- functor(HEAD, N, A).
clause_functor(HEAD, N, A) :- functor(HEAD, N, A).
