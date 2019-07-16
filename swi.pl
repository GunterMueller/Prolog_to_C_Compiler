%%% Support code for SWI Prolog


:- op(400,yfx,'\\\\').
:- op(1150,fx,determinate).
:- op(1150,fx,mode).

%% mode-/determinate-declarations are used in lib/rdtok and lib/read, so we define
%% a dummy predicate to treat it as a latent goal.
mode(_).
determinate(_).

command_line_arguments(ARGS) :-
	current_prolog_flag(argv, X),
        filter_dashes(X, ARGS),
        !.

% newer versions of SWI filter drop arguments before "--":
filter_dashes(X, ARGS) :-
	append(_, ['--'|ARGS], X).
filter_dashes(ARGS, ARGS).

dbreference(_) :- fail.		% sufficient here

enable_trace(_).

current_error_output(S) :- current_output(S).

%% this must produce identical results as hash_name() in pc.h
atom_hash(A, H) :-
	atom_codes(A, AL),
	atom_hash(0, AL, 0, H).
atom_hash(I, AL, H, H) :-
	(I >= 100; AL == []), !.
atom_hash(I, [C|R], H1, H2) :-
	H is (H1 xor ((H1 << 6) + (H1 >> 2) + C)) /\ 1073741823,
	I2 is I + 1,
	atom_hash(I2, R, H, H2).

skip_shebang :-	\+peek_char('#').
skip_shebang :- repeat, get0(10).
