main :-
	list_to_ord_set([1,2,6,foo, bar], S1),
	ord_disjoint(S1, [0,-1]),
	\+ord_disjoint(S1, [1]),
	ord_insert(S1, 0, [0|_]),
	ord_intersect([0,1],[1,2]),
	\+ord_intersect([0,1],[2,3]),
	ord_intersect(S1, [0,1,2], [1,2]),
	ord_subset([1,2],S1),
	\+ord_subset([1,3],S1),
	ord_subtract(S1, [1,2,6],[bar,foo]),
	ord_symdiff([1,2,3],[2,3,4],[1,4]),
	ord_union(S1, [a,b,c], X), display(X), nl.
