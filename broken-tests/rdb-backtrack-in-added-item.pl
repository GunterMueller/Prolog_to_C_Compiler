%% loop due to backtracking into a replaced rdb-entry

main :-
	recordz(count, 0),
	loop.

loop :-
	count,
	fail.
loop.

count :-
	recorded(count, N, REF), % [*]
	erase(REF),		% no next item
	N2 is N + 1,
	recordz(count, N2). % here: we add a next item, fail above will backtrack to [*]
