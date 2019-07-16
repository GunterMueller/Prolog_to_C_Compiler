%%%% I/O on strings


:- verbatim('#include "openstr.c"').


open_input_string(STR, STREAM) :-
	foreign_call(open_input_string(STR, [string, input, mode(read)], STREAM)).

open_output_string(STREAM) :-
	foreign_call(open_output_string([string, output, mode(write)], STREAM)).

get_output_string(STREAM, STR) :-
	foreign_call(stream_data(STREAM, DATA)),
	( member(data(PTR), DATA)
	; throw(type_error('string-port', STREAM))
	),
	foreign_call(get_output_string(STREAM, PTR, S1)),
	S1 \== 0,
	!,
	STR = S1.
get_output_string(_, STR) :-
	'$retry_string_to_list'(STR).
