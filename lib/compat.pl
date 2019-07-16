%%%% compatibility predicates for SWI


:- mode gensym(+, -),
	downcase_string(+, -),
	upcase_string(+, -).


gensym(BASE, ATM) :-
	( recorded(gensym_counter, C, REF), erase(REF)
	; C = 1
	),
	C2 is C + 1,
	recordz(gensym_counter, C2),
	atomic_list_concat([BASE, C], ATM),
	!.

%%XXX dog slow and incomplete
code_type(C, ascii) :- between(0, 127, C).
code_type(C, ctrl) :- between(0, 31, C).
code_type(C, space) :- member(C, [9, 10, 11, 12, 13, 32]).
code_type(C, lower) :- member(C, "abcdefghijklmnopqrstuvwxyz").
code_type(C, upper) :- member(C, "ABCDEFGHIJKLMNOPQRSTUVWXYZ").
code_type(C, alpha) :- code_type(C, lower); code_type(C, upper).
code_type(C, digit) :- member(C, "0123456789").
code_type(C, alnum) :- code_type(C, alpha); code_type(C, digit).
code_type(-1, end_of_file).
code_type(C, white) :- C = 9; C = 32.

char_type(C, T) :-
	var(C),
	!,
	code_type(N, T),
	char_code(C, N).
char_type(C, T) :-
	char_code(C, N),
	code_type(N, T).

downcase_atom(A, R) :-
	atom_codes(A, CS),
	downcase_string(CS, DCS),
	atom_codes(R, DCS).

downcase_string([], []).
downcase_string([C|R], [C2|R2]) :-
	( C >= 65, C =< 90	% 'A', 'Z'
	-> C2 is C + 32
	; C2 = C
	),
	!,
	downcase_string(R, R2).

upcase_atom(A, R) :-
	atom_codes(A, CS),
	upcase_string(CS, DCS),
	atom_codes(R, DCS).

upcase_string([], []).
upcase_string([C|R], [C2|R2]) :-
	( C >= 97, C =< 122	% 'a', 'z'
	-> C2 is C - 32
	; C2 = C
	),
	!,
	upcase_string(R, R2).
