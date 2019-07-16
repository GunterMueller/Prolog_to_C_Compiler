%% process was compiled as tail-call, which it shouldn't, as repeat/0 fill always
%% succeed again.

main :-
	generate.

generate :-
	repeat,
	process.

process :- fail.
