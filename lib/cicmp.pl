%%%% case-insensitive comparison of atoms


:- verbatim('#include "cicmp.c"').


compare_atoms_case_insensitive(C, A, B) :-
	foreign_call(compare_atoms_ci(A, B, R)),
	compare_atoms_case_insensitive(R, C).

compare_atoms_case_insensitive(0, =) :-
	!.
compare_atoms_case_insensitive(N, <) :-
	N < 0, !.
compare_atoms_case_insensitive(_, >).
