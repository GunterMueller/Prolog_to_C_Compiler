%%% ISO stuff


atom_concat(X, Y, Z) :-
	var(X),
	!,
	name(Y, YL), atom_codes(Z, ZL),
	append(XL, YL, ZL), name(X, XL).
atom_concat(X, Y, Z) :-
	var(Y),
	!,
	name(X, XL), atom_codes(Z, ZL),
	append(XL, YL, ZL), name(Y, YL).
atom_concat(X, Y, Z) :-
	name(X, XL), name(Y, YL),
	append(XL, YL, ZL), atom_codes(Z, ZL).

atom_chars(X, Y) :-
	var(X),
	!,
	findall(C, (member(CH, Y), char_code(CH, C)), YL),
	atom_codes(X, YL).
atom_chars(X, Y) :-
	atom_codes(X, XL),
	findall(CH, (member(C, XL), char_code(CH, C)), Y).

number_chars(X, Y) :-
	var(X),
	!,
	findall(C, (member(CH, Y), char_code(CH, C)), YL),
	number_codes(X, YL).
number_chars(X, Y) :-
	number_codes(X, XL),
	findall(CH, (member(C, XL), char_code(CH, C)), Y).

get_char(C) :- get_byte(C1), (C1 == -1 -> C = end_of_file; char_code(C, C1)).

get_char(S, C) :- get_byte(S, C1), (C1 == -1 -> C = end_of_file; char_code(C, C1)).

peek_char(C) :- peek_byte(C1), (C1 == -1 -> C = end_of_file; char_code(C, C1)).

peek_char(S, C) :- peek_byte(S, C1), (C1 == -1 -> C = end_of_file; char_code(C, C1)).

put_char(C) :- char_code(C, N), put_byte(N).

put_char(S, C) :- char_code(C, N), put_byte(S, N).
