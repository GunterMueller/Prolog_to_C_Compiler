main :- memberchk(123, [1,2,123,4,5]),
	\+memberchk(3, []),
	X = [1,2,3|Y].

