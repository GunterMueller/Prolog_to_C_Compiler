%%% POP_CHOICE_POINT didn't restore E, which broken in nested disjunctions

%% from interp.pl, by R. O'Keefe

trace(Goal) :-
	tr_goal(Goal, 0).

tr_goal(call(Goal), Depth) :- !,
	nonvar(Goal),
	tr_body(Goal, Depth).
tr_goal(\+(Goal), Depth) :-
	tr_body(Goal, Depth),
	!, fail.
tr_goal(\+(Goal), Depth) :- !.
tr_goal(Goal, Depth) :-
	(   tab(Depth), display('Call: '), writeq(Goal), nl, fail
	;   Depth1 is 1+Depth,
	    tr_call(Goal, Depth1),
	    (   tab(Depth), display('Exit: '), writeq(Goal), nl, fail
	    ;	true
	    ;   tab(Depth), display('Redo: '), writeq(Goal), nl, fail
	    )
	;   tab(Depth), display('Fail: '), writeq(Goal), nl, fail
	).


tr_call(Goal, Depth) :-
	system(Goal),
	!,
	call(Goal).
tr_call(Goal, Depth) :-
	clause(Goal, Body),
	tr_body(Body, Depth, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		tab(Depth), display('CUT'), nl,
		tr_body(AfterCut, Depth)
	;   HadCut = no
	).


tr_body(Body, Depth) :-
	tr_body(Body, Depth, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		tab(Depth), display('CUT'), nl,
		tr_body(AfterCut, Depth)
	;   HadCut = no
	).


tr_body((Conj1,Conj2), Depth, AfterCut, HadCut) :- !,
	tr_body(Conj1, Conj2, Depth, AfterCut, HadCut).
tr_body(!, _, true, yes) :- !.
tr_body((Disj1;_), Depth, AfterCut, HadCut) :-
	tr_body(Disj1, Depth, AfterCut, HadCut).
tr_body((_;Disj2), Depth, AfterCut, HadCut) :- !,
	tr_body(Disj2, Depth, AfterCut, HadCut).
tr_body(true, _, true, no) :- !.
tr_body(Goal, Depth, true, no) :-
	tr_goal(Goal, Depth).

tr_body(!, AfterCut, _, AfterCut, yes) :- !.
tr_body((A,B), Conj, Depth, AfterCut, HadCut) :- !,
	tr_body(A, (B,Conj), Depth, AfterCut, HadCut).
tr_body((Disj1;_), Conj, Depth, AfterCut, HadCut) :-
	tr_body(Disj1, Conj, Depth, AfterCut, HadCut).
tr_body((_;Disj2), Conj, Depth, AfterCut, HadCut) :- !,
	tr_body(Disj2, Conj, Depth, AfterCut, HadCut).
tr_body(true, Body, Depth, AfterCut, HadCut) :- !,
	tr_body(Body, Depth, AfterCut, HadCut).
tr_body(Goal, Body, Depth, AfterCut, HadCut) :-
	tr_goal(Goal, Depth),
	tr_body(Body, Depth, AfterCut, HadCut).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


system(true).
system(fail).
system(TERM) :-
	functor(TERM, NAME, ARITY),
	system_predicate(NAME, ARITY).

system_predicate(display, 1).

call(true) :- !.
call(fail) :- !, fail.
call(TERM) :-
	functor(TERM, NAME, ARITY),
	call_primitive(NAME, ARITY, TERM), !.
call(X) :- throw(error('can not call', X)).

call_primitive(display, 1, TERM) :- !, arg(1, TERM, X), display(X).

main :-
	assertz((p :- display(one), fail)),
	display('---\n'),
	show,
	trace(p).

show :-
	clause(p, X), writeq(X), nl, fail.
show.
