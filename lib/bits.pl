%%% operations on bit-strings (lists of 0/1)


%% convert string to list of bits
%
% length of bit-list must be a multiple of 8

string_bits([], []).
string_bits([N|R], B) :-
	code_bits(N, B, BR),
	string_bits(R, BR).

%% convert byte (code) to list of bits
%
% if L is given, then it must have 8 elements

code_bits(N, L) :-
	code_bits(N, L, []).

code_bits(N, L, R) :-
	var(N), !,
	'$code_bits'(7, L, N, R).
code_bits(N, L, R) :-
	code_bits(7, N, L, R).

'$code_bits'(-1, R, 0, R).
'$code_bits'(M, [B|R], N, R2) :-
	M2 is M - 1,
	'$code_bits'(M2, R, N1, R2),
	N is N1 \/ (B << M).

code_bits(-1, _, E, E) :- !.
code_bits(M, N, [B|R], R2) :-
	B is (N >> M) /\ 1,
	M2 is M - 1,
	code_bits(M2, N, R, R2).

%% convert positive number to list of bits

number_bits(N, L) :-
	var(N), !,
	'$number_bits'(L, S, N).
number_bits(N, L) :-
	number_bits(N, L, []).

'$number_bits'([], 0, 0).
'$number_bits'([B|L], S, N) :-
	'$number_bits'(L, S1, N1),
	S is S1 + 1,
	N is (B << S1) \/ N1.

number_bits(0, R, R).
number_bits(N, L, R) :-
	N > 0,
	B is N /\ 1,
	N2 is N >> 1,
	number_bits(N2, L, [B|R]).
