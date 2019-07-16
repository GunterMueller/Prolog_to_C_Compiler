%%%% operations on file-descriptors and raw I/O


:- verbatim('#include "fd.c"').


write_bytes(_, []) :- !.
write_bytes(_, '') :- !.
write_bytes(FD, DATA) :-
	atom(DATA),
	!,
	foreign_call(raw_write(FD, DATA, N)),
	sub_atom(DATA, N, LEN, 0, REST),
	write_bytes(FD, REST).
write_bytes(FD, DATA) :-
	foreign_call(raw_write(FD, DATA, N)),
	skip_bytes(N, DATA, REST),
	!,
	write_bytes(FD, REST).


%% will return [] on EOF, and block after that

read_bytes(FD, LEN, BYTES) :-
	foreign_call(raw_read(FD, LEN, BYTES)).


%% probably generally useful, find out how this is usually called

skip_bytes(0, _, []).
skip_bytes(N, [_|R], R2) :-
	N2 is N - 1,
	!,
	skip_bytes(N2, R, R2).


%% open file-descriptor

open_fd(FD, MODE, STREAM) :-
	open_fd(FD, MODE, STREAM, []).

open_fd(FD, write, STREAM, OPTIONS) :-
	open_fd(FD, 0, "w", OPTIONS, [output, mode(write)], STREAM).
open_fd(FD, read, STREAM, OPTIONS) :-
	open_fd(FD, 1, "r", OPTIONS, [input, mode(read)], STREAM).
open_fd(FD, append, STREAM, OPTIONS) :-
	open_fd(FD, 0, "a", OPTIONS, [output, mode(append)], STREAM).

open_fd(FD, INPUT, MODE, [], DATA, STREAM) :-
	name(M, MODE),
	foreign_call(open_fd(FD, INPUT, M, [file_no(FD)|DATA], STREAM)).
open_fd(FD, INPUT, MODE, [type(text)|MORE], DATA, STREAM) :-
	open_fd(FD, INPUT, MODE, MORE, [type(text)|DATA], STREAM).
open_fd(FD, INPUT, MODE, [type(binary)|MORE], DATA, STREAM) :-
	append(MODE, "b", MODE2),
	open_fd(FD, INPUT, MODE2, MORE, [type(binary)|DATA], STREAM).
open_fd(FD, INPUT, MODE, [_|MORE], DATA, STREAM) :-
	open_fd(FD, INPUT, MODE, MORE, DATA, STREAM).


%% wait for I/O on file-descriptors

poll_fds(FDS, READY) :-
	poll_fds(FDS, -1, READY).

poll_fds(FDS, TIMEOUT, READY) :-
	foreign_call(poll_fds(FDS, TIMEOUT, READY)).


%% generally useful, but it seldom works for me...

set_stream_buffer(STR, BUF) :-
	foreign_call(set_stream_buffer(STR, BUF)).
