%%%% support predicates for freeze/2


'$delay_goal'(VAR, PRIO, PTR, ARGS) :-
	( var(VAR) -> foreign_call(delay_goal(VAR, PRIO, PTR, ARGS))
	; '$call'(PTR, ARGS)
	).

'$freeze_goal'(VAR, PRIO, PTR, ARGS) :-
	( var(VAR) ->
	  '$predicate_address'('$freeze_goal'/4, PTR2),
	  delay(VAR, '$call'(PTR2, [VAR, PRIO, PTR, ARGS]))
	; '$call'(PTR, ARGS)
	).

dif(X, Y) :-
	foreign_call(special_id(X, Y, V)),
	!,
	(var(V) -> delay(V, dif(X, Y), 2)).
dif(_, _).
