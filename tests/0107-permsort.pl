/* Generate all permutations. Implementation suited to coroutining. */
permsort([], []).
permsort([H|T], P) :-
	length([H|T], L),
	length(P, L),
	freeze_order(P),
	permute([H|T], P).	% i.e. create a list of free variables

/* Auxiliary predicate: generate permutation when free-variable skeleton is given. */
permute([], []).
permute([H|T], [TemplateH|TemplateT]) :-
	select(Elem, [H|T], Rem),
	TemplateH = Elem,
	permute(Rem, TemplateT).

/* Coroutine that checks whether order is maintained in a list of free variables. */
freeze_order([_]). % single element is always ordered
freeze_order([H|[HT|TT]]) :-
	freeze(HT, HT >= H),
	freeze_order([HT|TT]).


main :-
	permsort([6,22,5,7,1,66,9,10,11], X),
	write(X), nl.


/* normal permsort (mcuh slower):

permute([], []).
permute([H|T], [RH|RTT]) :- select(RH, [H|T], RT), permute(RT, RTT).

permsort(L, S) :- permute(L, S), sort(L, S).
*/
