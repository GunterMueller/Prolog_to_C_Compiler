%%% cycle-detection in trees, from the paper
%
% "A Portable Prolog Predicate for Printing Rational Terms"
% (Mantadelis + Rocha)


canonical_term(Term, Canonical, Print) :-
	Term =.. InList,
	decompose_cyclic_term(Term, InList, OutList, OpenEnd, [Term],
			      PrintList-Cycle_mark, 0),
	Canonical =.. OutList,
	Canonical = OpenEnd,
	Print =.. PrintList,
	Cycle_mark = cycle_at_depth(0).

decompose_cyclic_term(_, [], [], _, _, []-_, _).
decompose_cyclic_term(CyclicTerm, [Term|Tail], [Term|NewTail], OpenEnd,
		      Stack, [Term|NewPrintTail]-Cycle_mark, DepthCount) :-
	acyclic_term(Term), !,
	decompose_cyclic_term(CyclicTerm, Tail, NewTail, OpenEnd, Stack,
			      NewPrintTail-Cycle_mark, DepthCount).
decompose_cyclic_term(CyclicTerm, [Term|Tail], [OpenEnd|NewTail], OpenEnd,
		      Stack, [Cycle_mark|NewPrintTail]-Cycle_mark, DepthCount) :-
	CyclicTerm == Term, !,
	decompose_cyclic_term(CyclicTerm, Tail, NewTail, OpenEnd, Stack,
			      NewPrintTail-Cycle_mark, DepthCount).
decompose_cyclic_term(CyclicTerm, [Term|Tail], [Canonical|NewTail], OpenEnd,
		      Stack, [Print|NewPrintTail]-Cycle_mark, DepthCount) :-
	\+ instack(Term, Stack), !,
	Term =.. InList,
	NewDepthCount is DepthCount + 1,
	decompose_cyclic_term(Term, InList, OutList, OpenEnd2, [Term|Stack],
			      PrintList-Cycle_mark_2, NewDepthCount),
	Canonical =.. OutList,
	Print =.. PrintList,
	( Canonical = OpenEnd2,
	  Canonical == Term,
	  Cycle_mark_2 = cycle_at_depth(NewDepthCount),
	  !
	; OpenEnd2 = OpenEnd,
	  Cycle_mark_2 = Cycle_mark
	),
	decompose_cyclic_term(CyclicTerm, Tail, NewTail, OpenEnd, Stack,
			      NewPrintTail-Cycle_mark, DepthCount).
decompose_cyclic_term(CyclicTerm, [_|Tail], [OpenEnd|NewTail], OpenEnd,
		      Stack, [Cycle_mark|NewPrintTail]-Cycle_mark, DepthCount) :-
	decompose_cyclic_term(CyclicTerm, Tail, NewTail, OpenEnd, Stack,
			      NewPrintTail-Cycle_mark, DepthCount).

instack(E, [H|_]) :- E == H, !.
instack(E, [_|T]) :- instack(E, T).
