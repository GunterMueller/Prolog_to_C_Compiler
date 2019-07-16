%%% simpler shell commands
%
% X, Y     -->   X " " Y
% X <OP> Y -->   X "<OP>" Y
% LIST   -->     STRING
% ATOM, NUMBER -->  "<ATOMIC>"
% <OP> X   -->   "<OP>" X
% <NAME>(X, ...)  --> "<NAME> " " X " " ...
% (X)      -->   X
% []       -->


run(CMD) :-
	run(CMD, 0).

run(CMD, S) :-
	run_translate(CMD, STR, []),
	!,
	shell(STR, S).

run_insert(LST, OUT, IN) :- append(LST, IN, OUT).

run_translate([]) --> "".
run_translate(A) --> {atomic(A)}, !, {name(A, STR)}, run_insert(STR).
run_translate(X) --> {X = [_|_]}, !, run_insert(X).
run_translate((A, B)) --> run_translate(A), " ", run_translate(B).
run_translate(S) --> {S =.. [N|ARGS]}, run_translate_op(N, ARGS).

run_translate_op(OP, [X, Y]) -->
	{current_op(_, _, OP)}, !, run_translate(X), run_translate(OP), run_translate(Y).
run_translate_op(OP, [X]) -->
	{current_op(_, _, OP)}, !, run_translate(OP), run_translate(X).
run_translate_op(OP, ARGS) -->
	run_translate(OP), " ", run_translate_list(ARGS).

run_translate_list([]) --> "".
run_translate_list([X]) --> run_translate(X).
run_translate_list([X|R]) --> run_translate(X), " ", run_translate_list(R).

temporary_file(FN) :-
	getpid(PID), R is random(1000), C is clock,
	(getenv('TMPDIR', TMPDIR); TMPDIR = '/tmp'),
	atomic_list_concat([TMPDIR, '/tmp', C, '.', R, '.', PID], FN).

capture(CMD, OUTPUT) :-
	temporary_file(TMP),
	run((CMD, '>', TMP)),
	open(TMP, read, IN),
	read_string(IN, all, OUTPUT1),
	close(IN),
	delete_file(TMP),
	split_string(OUTPUT1, "", " \t\”", [OUTPUT]).
