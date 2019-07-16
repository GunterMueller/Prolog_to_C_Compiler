%%% support code for findall/3


:- mode '$findall_collect'(+, +, -),
	'$unbound_variables'(+, -),
	'$bagof_list_instances'(+, +, +, +, -),
	'$bagof_replace_key_variables'(+, +, +),
	'$concordant_subset'(+, +, -),
	'$concordant_subset'(+, +, -, -),
	'$concordant_subset'(+, +, +, +, -).


:- global_variable(findall_solutions).
:- global_variable(bagof_info).
:- pre_initialization(global_set(findall_solutions, [])).
:- pre_initialization(global_set(bagof_info, [])).


%% findall/3

'$findall_start' :- '$findall_push'('$<mark>').

'$findall_push'(X) :-
	!,
	global_ref(findall_solutions, L),
	copy_term(X, X2),
	deref_term([X2|L], 2, 0, X3),
	global_set(findall_solutions, X3).

'$findall_collect'(L) :-
	global_ref(findall_solutions, SL),
	'$findall_collect'(SL, [], L).

'$findall_collect'([X|MORE], R, L) :-
	(var(X); X \== '$<mark>'),
	!, '$findall_collect'(MORE, [X|R], L).
'$findall_collect'([_|MORE], R1, R2) :-
	%% first update global before unifying
	global_set(findall_solutions, MORE),
	R1 = R2.


%% bagof/3, setof/3

'$bagof_start'(VARS, TEMPLATE, TEMPLATE2) :-
	'$unbound_variables'(VARS, UVARS),
	!,
	'$bagof_start_unbound'(UVARS, TEMPLATE, TEMPLATE2).

'$bagof_start_unbound'(UVARS, TEMPLATE, KEY-TEMPLATE) :-
	global_ref(bagof_info, OLD),
	KEY =.. ['.'|UVARS],
	deref_term([KEY|OLD], 2, 0, BI),
	global_set(bagof_info, BI),
	'$findall_push'('$<mark>'),
	!.

'$bagof_finish'(BAG) :-
	global_ref(bagof_info, [KEY|OLD]),
	global_set(bagof_info, OLD),
	functor(KEY, '.', N),
	'$findall_collect'(SL),
	'$bagof_list_instances'(SL, KEY, N, [], OmniumGatherum),
	keysort(OmniumGatherum, Gamut), !,
	'$concordant_subset'(Gamut, KEY, BAG).

'$unbound_variables'([], []).
'$unbound_variables'([X|R], [X|R2]) :-
	var(X),
	!,
	'$unbound_variables'(R, R2).
'$unbound_variables'([_|R], R2) :-
	'$unbound_variables'(R, R2).

%   '$bagof_list_instances'(Key, NVars, BagIn, BagOut)
%   pulls all the Key-Template instances out of the data base until
%   it hits the marker.  The Generator should not touch recordx(.,_,_).
%   Note that asserting something into the data base and pulling it out
%   again renames all the variables; to counteract this we use replace_
%   key_variables to put the old variables back.  Fortunately if we
%   bind X=Y, the newer variable will be bound to the older, and the
%   original key variables are guaranteed to be older than the new ones.
%   This replacement must be done @i<before> the keysort.

'$bagof_list_instances'([NEWKEY-TERM|MORE], KEY, NVARS, BAGIN, BAGOUT) :-
	'$bagof_replace_key_variables'(NVARS, KEY, NEWKEY),
	!,
	'$bagof_list_instances'(MORE, KEY, NVARS, [NEWKEY-TERM|BAGIN], BAGOUT).
'$bagof_list_instances'([], _, _, BAG, BAG).


%   There is a bug in the compiled version of arg in Dec-10 Prolog,
%   hence the rather strange code.  Only two calls on arg are needed
%   in Dec-10 interpreted Prolog or C-Prolog.

'$bagof_replace_key_variables'(0, _, _) :- !.
'$bagof_replace_key_variables'(N, OldKey, NewKey) :-
	arg(N, NewKey, Arg),
	nonvar(Arg), !,
	M is N-1,
	'$bagof_replace_key_variables'(M, OldKey, NewKey).
'$bagof_replace_key_variables'(N, OldKey, NewKey) :-
	arg(N, OldKey, OldVar),
	arg(N, NewKey, OldVar),
	M is N-1,
	'$bagof_replace_key_variables'(M, OldKey, NewKey).


%   '$concordant_subset'([Key-Val list], Key, [Val list]).
%   takes a list of Key-Val pairs which has been keysorted to bring
%   all the identical keys together, and enumerates each different
%   Key and the corresponding lists of values.

'$concordant_subset'([Key-Val|Rest], Clavis, Answer) :-
	'$concordant_subset'(Rest, Key, List, More),
	'$concordant_subset'(More, Key, [Val|List], Clavis, Answer).


%   '$concordant_subset'(Rest, Key, List, More)
%   strips off all the Key-Val pairs from the from of Rest,
%   putting the Val elements into List, and returning the
%   left-over pairs, if any, as More.

'$concordant_subset'([Key-Val|Rest], Clavis, [Val|List], More) :-
	Key == Clavis,
	!,
	'$concordant_subset'(Rest, Clavis, List, More).
'$concordant_subset'(More, _, [], More).


%   '$concordant_subset'/5 tries the current subset, and if that
%   doesn't work if backs up and tries the next subset.  The
%   first clause is there to save a choice point when this is
%   the last possible subset.

'$concordant_subset'([],   Key, Subset, Key, Subset) :- !.
'$concordant_subset'(_,    Key, Subset, Key, Subset).
'$concordant_subset'(More, _,   _,   Clavis, Answer) :-
	'$concordant_subset'(More, Clavis, Answer).
