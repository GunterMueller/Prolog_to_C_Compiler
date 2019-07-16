%%% misc support library


gen(T) :- display(T).
gen(T1, T2) :- gen(T1), gen(T2).
gen(T1, T2, T3) :- gen(T1, T2), gen(T3).
gen(T1, T2, T3, T4) :- gen(T1, T2, T3), gen(T4).
gen(T1, T2, T3, T4, T5) :- gen(T1, T2, T3, T4), gen(T5).

gen_list([]).
gen_list([X|R]) :- gen(X), !, gen_list(R).

error(MSG) :-
	display(user_error, 'ERROR: '),
	forall(member(X, MSG), write(user_error, X)),
	display(user_error, '\n\n'),
	halt(1).

message(MSG) :-
	(recorded(silent, yes)
	; forall(member(X, MSG), write(X)), nl
	),
	!.

iota(N, L) :- iota(0, N, L).
iota(N, N, []) :- !.
iota(N, M, [N|R]) :-
	N2 is N + 1, iota(N2, M, R).

file_name_string(IFILE, F) :- append(F, [46|_], IFILE), !.
file_name_string(IFILE, IFILE).

mangle_name(NAME, MNAME) :-
	name(NAME, STRING),
	findall(CS, (member(C, STRING), mangle_char(C, CS)), MSTRING),
	append(["___"|MSTRING], MSTRING2),
	name(MNAME, MSTRING2),
	!.

mangle_char(C, [C]) :- (C >= 97, C =< 122; C >= 48, C =< 57), !.
mangle_char(C1, [95, C2, C3]) :-
	N is C1 // 16, hexdigit(N, C2),
	M is C1 - N * 16, hexdigit(M, C3).

hexdigit(N, M) :- N < 10, M is N + 48, !.
hexdigit(N, M) :- M is N + 87.

%% extract second arg of terms in list - this doesn't use findall/3,
%% to avoid renaming
map_second([], []).
map_second([T|MORE], [V|REST]) :-
	arg(2, T, V),
	map_second(MORE, REST).

%%% locating files

locate_file(library(FN), RNAME) :-
	recorded(library_dir, DIR),
	atomic_list_concat([DIR, '/', FN], FN2),
	locate_file(FN2, RNAME).
locate_file(NAME, RNAME) :- atom_concat(NAME, '.pl', RNAME), exists_file(RNAME), !.
locate_file(NAME, NAME) :- exists_file(NAME), !.
locate_file(NAME, _) :- error(['file not found: ', NAME]).

