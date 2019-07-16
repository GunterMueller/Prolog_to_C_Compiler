main :-
	see('tests/0067-rdtok.pl'),
	read_tokens(TOKS, VARS),
	write(TOKS), nl,
	write(VARS), nl,
	seen.
