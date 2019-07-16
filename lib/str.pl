%%%% string- and atom utilities


:- mode '$atomic_list_concat'(+, -),
	'$split_string_split'(+, +, -),
	'$split_string_trim'(+, +, -),
	'$split_string_trim_head'(+, +, -),
	'$split_string_trim_tail'(+, +, -).
			      

%% join atomics

atomic_list_concat(LST, ATM) :-
	'$atomic_list_concat'(LST, STR),
	atom_codes(ATM, STR).

'$atomic_list_concat'([], "").
'$atomic_list_concat'([X|R], L) :-
	'$atomic_list_concat'(R, R2), 
	name(X, XL), append(XL, R2, L).


%% split string into substrings

split_string(STR, SEP, PAD, SUB) :-
	'$split_string_split'(STR, SEP, S1),
	findall(Y, (member(X, S1), X \== [], '$split_string_trim'(X, PAD, Y)), SUB).

'$split_string_split'([], _, [""]).
'$split_string_split'([C|L], SEP, [""|R]) :-
	memberchk(C, SEP),
	!,
	'$split_string_split'(L, SEP, R).
'$split_string_split'([C|L], SEP, [[C]|R2]) :-
	'$split_string_split'(L, SEP, [""|R2]),
	!.
'$split_string_split'([C|L], SEP, [[C|W]|R2]) :-
	'$split_string_split'(L, SEP, [W|R2]).

'$split_string_trim'(L, "", L) :- !.
'$split_string_trim'(L, SET, R) :-
	'$split_string_trim_head'(L, SET, L1),
	'$string_split_trim_tail'(L1, SET, R).

'$split_string_trim_head'("", _, "").
'$split_string_trim_head'([C|R], SET, R2) :-
	memberchk(C, SET),
	!,
	'$split_string_trim_head'(R, SET, R2).
'$split_string_trim_head'(R, _, R).

'$string_split_trim_tail'("", _, "").
'$string_split_trim_tail'([C|R1], SET, R) :-
	'$string_split_trim_tail'(R1, SET, R2),
	( R2 == "", memberchk(C, SET)
	-> R = ""
	; R = [C|R2]
	).


%% compute sub-atom

sub_atom(X, _, _, _, _) :-
	var(X), throw(instantiation_error).
sub_atom(ATOM, BEFORE, LEN, A2, SATOM) :-
	atom_length(ATOM, ALEN),
	ALEN2 is ALEN - 1,
	between(0, ALEN, BEFORE),
	between(0, ALEN, AFTER), A2 is ALEN - AFTER,
	LEN is ALEN - A2 - BEFORE,
	LEN >= 0,
	foreign_call(sub_atom(ATOM, BEFORE, LEN, SATOM)).
