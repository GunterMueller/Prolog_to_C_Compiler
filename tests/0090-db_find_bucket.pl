main :-
	global_ref(record_db, DB),
	recordz(foo, 123),
	recordz(bar, 456),
	foreign_call(db_find_bucket(DB, no, K1, R1)),
	foreign_call(db_ref(R1, X)), display([K1, X]), nl,
	foreign_call(db_find_bucket(DB, R1, K2, R2)),
	foreign_call(db_ref(R2, Y)), display([K2, Y]), nl,
	foreign_call(db_find_bucket(DB, R2, K3, R3)),
	foreign_call(db_ref(R3, Z)), display([K3, Z]), nl.
