%%% record database operations


:- global_variable(record_db).
:- pre_initialization((foreign_call(db_create(record_db, 3001, DB)),
		       global_set(record_db, DB))).

:- determinate '$record_db_key'/2,
	'$record'/4.


recorda(KEY, TERM) :- recorda(KEY, TERM, _).
recorda(KEY, TERM, REF) :- '$record'(KEY, TERM, REF, 0).
recordz(KEY, TERM) :- recordz(KEY, TERM, _).
recordz(KEY, TERM, REF) :- '$record'(KEY, TERM, REF, 1).

'$record'(KEY, TERM, REF, ATEND) :-
	global_ref(record_db, DB),
	'$record_db_key'(KEY, KEY2),
	foreign_call(db_record(DB, ATEND, KEY2, TERM, REF)).

recorded(KEY, TERM) :-
	recorded(KEY, TERM, _).
recorded(KEY, TERM, REF) :-
	nonvar(REF),
	!,
	'$record_db_match'(REF, TERM, REF).
recorded(KEY, TERM, REF) :-
	global_ref(record_db, DB),
	'$record_db_key'(KEY, KEY2),
	foreign_call(db_find(DB, KEY2, REF1)),
	!,
	'$record_db_match'(REF1, TERM, REF).

'$record_db_match'(REF, TERM, REF) :-
	( foreign_call(db_ref(REF, X))
	-> X = TERM
	; garbage_collect,
	  '$record_db_match'(REF, TERM, REF)
	).
'$record_db_match'(REF1, TERM, REF) :-
	foreign_call(db_next(REF1, REF2)),
	!,
	'$record_db_match'(REF2, TERM, REF).

'$record_db_key'(K1, K2) :-
	compound(K1),
	!,
	foreign_call(cdb_key(K1, K2)).
'$record_db_key'(K, K).
