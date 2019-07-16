%%% copying data blobs from file to file


:- verbatim('#include "fcopy.c"').


file_copy(IN, OUT) :-
	file_copy(-1, IN, OUT).

file_copy(LEN, IN, OUT) :-
	foreign_call(file_copy(LEN, IN, OUT)).
