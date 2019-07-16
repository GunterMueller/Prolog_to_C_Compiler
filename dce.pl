%%% eliminate unused predicates


%% iterate over defined predicates and collect callers, dropping the
%% predicate if none exists.
mark_unused_predicates :-
	recordz(dce_count, 0),
	mark_unused_predicates_iterate(0),
	recorded(dce_count, COUNT),
	message(['% dropping ', COUNT, ' unused predicates']).

mark_unused_predicates_iterate(COUNT1) :-
	mark_unused_predicates_iterate,
	recorded(dce_count, COUNT2),
	COUNT2 > COUNT1,
	!,
	mark_unused_predicates_iterate(COUNT2).
mark_unused_predicates_iterate(_).

mark_unused_predicates_iterate :-
	recorded(defined, NA, REF),
	predicate_callers(NA, []),
	erase(REF),
	eliminate_predicate(NA),
	fail.
mark_unused_predicates_iterate.	

eliminate_predicate(NA) :-
	%%write(NA), nl,
	recorded(calls, calls(NA, _), REF),
	erase(REF),
	fail.
eliminate_predicate(_) :-
	recorded(dce_count, N, REF),
	erase(REF),
	N2 is N + 1,
	recordz(dce_count, N2),
	!.			% avoid backtracking (this is a bug)
