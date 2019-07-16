main :-
	open('iotest', write, OUT, type(binary)),
	tell(OUT),
	put(1), put(2), put(3),
	told,
	open('iotest', append, OUT2, type(binary)),
	tell(OUT2),
	put(4), put(5), put(6),
	told,
	open('iotest', read, OUT, type(binary)),
	see(OUT),
	between(1, 6, I), get(I), fail.
main :- seen.
