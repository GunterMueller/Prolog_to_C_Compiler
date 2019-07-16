:- include(library(openstr)).

main :-
	open_input_string('%...\nfoo("a", 123). ', S),
	read(S, X),
	writeq(X), nl,
	open_output_string(S2),
	fwritef(S2, "%d skiddoo\n%f", [23]),
	get_output_string(S2, Y),
	writef("%s", [Y]).

