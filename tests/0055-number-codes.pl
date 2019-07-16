main :-
	number_codes(123, "123"),
	number_codes(12.3, "12.3"),
	\+number_codes(_, "12a"),
	\+number_codes(_, "99999999999999+e99999999999999999999"),
	number_codes(X, "-0.45"), X =:= -0.45.
