main :-
	display('current_input:\n'), props(current_input),
	display('current_input:\n'), props(current_output),
	open('tests/0121-streamprops.pl', read, S),
	display('file:\n'), props(S),
	close(S).

props(S) :-
	stream_property(S, P),
	(P = file_no(N), N > 2 -> display(file_no('?')); display(P)),
	nl, fail.
props(_).
