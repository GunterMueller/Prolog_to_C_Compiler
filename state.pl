%%% compiler-state management


%% state(LABELCOUNTER, LITERALCOUNTER, OPENFILESTACK)

initial_state(state(1, 1, [])).


%% generate unique labels or literal indices

gen_label(L, S1, S2) :-	gensym('L', L, S1, S2).

gen_literal_index(S1, state(X, S1, Y), state(X, S2, Y)) :-
	S2 is S1 + 1.

gensym(PREFIX, N, state(S1, X, Y), state(S2, X, Y)) :-
	S2 is S1 + 1,
	atomic_list_concat([PREFIX, S1], N).


%% push and pop files on to/from file stack
%
% - works in both directions: STATE1 is without, STATE2 is with added element

%XXX can this be done with select/3 ?
open_file_stack(F, state(X, Y, Z), state(X, Y, [F|Z])).
