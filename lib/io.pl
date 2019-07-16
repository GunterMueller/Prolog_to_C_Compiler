%%% basic I/O - Edinburgh'ish


see(ALIAS) :-
	(ALIAS == user; ALIAS == user_input; ALIAS = current_input),
	!, foreign_call(set_current_input_stream(user_input)).
see(S) :-
	is_stream(S),
	!,
	foreign_call(set_current_input_stream(S)).	
see(NAME) :-
	foreign_call(open_stream(NAME, 1, 'rb', [], S)),
	foreign_call(set_current_input_stream(S)).

seen :-
	foreign_call(current_input_stream(S)),
	foreign_call(close_stream(S)).

tell(user) :-
	!, foreign_call(set_current_output_stream(user_output)).
tell(ALIAS) :-
	(ALIAS == user_output; ALIAS == user_error; ALIAS == current_output),
	!, foreign_call(set_current_output_stream(ALIAS)).
tell(S) :-
	is_stream(S), 
	!,
	foreign_call(set_current_output_stream(S)).
tell(NAME) :-
	foreign_call(open_stream(NAME, 0, 'wb', [], S)),
	foreign_call(set_current_output_stream(S)).

told :-
	foreign_call(current_output_stream(S)),
	foreign_call(close_stream(S)).

append(NAME) :-
	foreign_call(open_stream(NAME, 0, 'ab', [], S)),
	foreign_call(set_current_output_stream(S)).

tab(N) :- tab(current_output, N).

tab(S, N) :- N > 0, !, put(S, 32), N2 is N - 1, tab(S, N2).
tab(_, _).

get(C) :- get(current_input, C).

get(S, C) :-
	!, get0(S, C2),
	(C2 =:= -1, C = -1
	; C2 =\= 32, C = C2
	; get(S, C)).

skip(C) :- skip(current_input, C).

skip(S, C) :-
	!, get0(S, C2),
	(C2 =:= -1
	; C == C2
	; skip(S, C)).

open(NAME, MODE, STREAM) :- open(NAME, MODE, STREAM, []).

open(NAME, write, STREAM, OPTIONS) :-
	open(NAME, 0, "w", OPTIONS, [output, mode(write)], STREAM).
open(NAME, read, STREAM, OPTIONS) :-
	open(NAME, 1, "r", OPTIONS, [input, mode(read)], STREAM).
open(NAME, append, STREAM, OPTIONS) :-
	open(NAME, 0, "a", OPTIONS, [output, mode(append)], STREAM).

open(NAME, INPUT, MODE, [], DATA, STREAM) :-
	name(M, MODE),
	foreign_call(open_stream(NAME, INPUT, M, [file_name(NAME)|DATA], STREAM)).
open(NAME, INPUT, MODE, [type(text)|MORE], DATA, STREAM) :-
	open(NAME, INPUT, MODE, MORE, [type(text)|DATA], STREAM).
open(NAME, INPUT, MODE, [type(binary)|MORE], DATA, STREAM) :-
	append(MODE, "b", MODE2),
	open(NAME, INPUT, MODE2, MORE, [type(binary)|DATA], STREAM).
open(NAME, INPUT, MODE, [_|MORE], DATA, STREAM) :-
	open(NAME, INPUT, MODE, MORE, DATA, STREAM).

read_string(LEN, STR) :-
	read_string(current_input, LEN, STR).

read_string(STREAM, LEN, STR) :-
	foreign_call(read_string(STREAM, LEN, S1)),
	S1 \== 0,
	!,
	STR = S1.
read_string(_, _, STR) :-
	'$retry_string_to_list'(STR).

read_line(ATM) :-
	read_line(current_input, ATM).

read_line(STREAM, STR) :-
	foreign_call(read_line(STREAM, S1)),
	S1 \== 0,
	!,
	STR = S1.
read_line(_, STR) :-
	'$retry_string_to_list'(STR).

'$retry_string_to_list'(STR) :-
	foreign_call(retry_string_to_list(STR)).

set_input(S) :-
	foreign_call(set_current_input_stream(S)).
set_output(S) :-
	foreign_call(set_current_output_stream(S)).

flush_output :-
	foreign_call(current_output_stream(S)), foreign_call(flush_output(S)).

flush_output(S) :-
	foreign_call(flush_output(S)).

at_end_of_stream :-
	foreign_call(current_output_stream(S)), foreign_call(at_eof(S)).

at_end_of_stream(S) :-
	foreign_call(at_eof(S)).

set_stream_position(S, P) :-
	(P == end -> P2 = -1; P2 = P),
	foreign_call(set_stream_position(S, P2)).

'$stream_property'(position(P), S) :-
	foreign_call(stream_position(S, P)).
'$stream_property'(tty(B), S) :-
	(foreign_call(tty_stream(S)) -> B = true; B = false).
'$stream_property'(file_no(N), S) :-
	foreign_call(stream_fileno(S, N)).
'$stream_property'(P, S) :-
	foreign_call(stream_data(S, DATA)),
	member(P, DATA).
