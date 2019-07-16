%% read a big file

main :-
	see('tests/wn_s.pl'),
	repeat,
	read(X),
	X == end_of_file,
	seen.
