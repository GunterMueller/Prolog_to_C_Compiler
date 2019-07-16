main :-
	ord_memberchk(1,[1,2,3]),
	ord_memberchk(5,[1,2,4,5]),
	\+ord_memberchk(0,[1,2,4,5]),
	\+ord_memberchk(3,[1,2,4,5]),
	\+ord_memberchk(abc,[]).

	