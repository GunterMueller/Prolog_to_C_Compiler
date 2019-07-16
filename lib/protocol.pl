%%% parse binary protocol


:- ensure_loaded(library(bits)).


%% parse_binary_block(FORMAT, STRING, DATA)
%% parse_binary_block(FORMAT, STRING, REMBITS, REMSTRING, DATA)
%
% where
%
%   FORMAT = [FIELDSPEC, ...]
%   FIELDSPEC = field(BITCOUNT, NAME)
%
%   DATA = [FIELD, ...]
%   FIELD = NAME-VALUE

parse_binary_block(FORMAT, STRING, DATA) :-
	parse_binary_block(FORMAT, STRING, [], [], DATA).

parse_binary_block(FORMAT, STRING, REST, STRING2, DATA) :-
	parse_binary_block(FORMAT, [], STRING, REST, STRING2, DATA).
	    
parse_binary_block([], R, S, R, S, []).
parse_binary_block([field(BITS, NAME)|FORMAT], REST, STRING, REST2, STRING2, [NAME-VAL|DATA]) :-
	length(BITVALS, BITS),
	scan_field(BITVALS, NAME, REST, STRING, REST3, STRING3),
	number_bits(VAL, BITVALS),
	!,
	parse_binary_block(FORMAT, REST3, STRING3, REST2, STRING2, DATA).

scan_field([], _, R, S, R, S) :- !.
scan_field([BIT|BITS], NAME, [], [CODE|S], R2, S2) :-
	code_bits(CODE, CBITS),
	!,
	scan_field([BIT|BITS], NAME, CBITS, S, R2, S2).
scan_field([BIT|BITS], NAME, [BIT|REST], S, R2, S2) :-
	!,
	scan_field(BITS, NAME, REST, S, R2, S2).

build_binary_block(FORMAT, DATA, STRING) :-
	build_binary_block_bits(FORMAT, DATA, BITS),
	string_bits(STRING, BITS).

build_binary_block_bits([], [], []) :- !.
build_binary_block_bits([], LEFT, _) :-
	throw(error('unidentified data fields left', LEFT)).
build_binary_block_bits([field(W, NAME)|MORE], DATA, BITS) :-
	select(NAME-VAL, DATA, DATA2),
	build_field_bits(VAL, W, BITS, R),
	!,
	build_binary_block_bits(MORE, DATA2, R).
build_binary_block_bits([field(_, NAME)|_], _, _) :-
	throw(error('missing data field', NAME)).

build_field_bits(NUM, W, BITS, R) :-
	integer(NUM),
	number_bits(NUM, NBITS),
	length(NBITS, NW),
	adjust_bits(NW, W, NBITS, BITS, R).
build_field_bits(LST, W, BITS, R) :-
	length(LST, NB),
	(W == NB; throw(error('non-numeric bit-sequence has wrong length', NB, W))),
	append(LST, R, BITS).

adjust_bits(0, 0, [], R, R).	% done
adjust_bits(NW, W, [_|BITS], NEWBITS, R) :- 
	NW > W,			% more bits than required - drop
	NW2 is NW - 1,
	adjust_bits(NW2, W, BITS, NEWBITS, R).
adjust_bits(NW, W, BITS, [0|NEWBITS], R) :-
	NW < W,			% less bits than required - pad on left with 0
	W2 is W - 1,
	adjust_bits(NW, W2, BITS, NEWBITS, R).
adjust_bits(W, W, [BIT|BITS], [BIT|NEWBITS], R) :-
	W2 is W - 1,
	adjust_bits(W2, W2, BITS, NEWBITS, R).
			   
/*
format([field(3, a), field(4, b), field(16, c), field(1, d)]).

main :-
	format(F),
	parse_binary_block(F, "0123", R, S, D),
	writeq(D), nl,
	writeq(R), nl,
	writeq(S), nl,
	gen.

gen :-
	format(F),
	build_binary_block(F, [a-2, c-100, b-[0,1,0,1], d-15], X),
	writeq(X), nl,
	string_bits(X, S), write(S), nl.
*/
