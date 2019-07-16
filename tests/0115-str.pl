main :-
	split_string("//home/foo///bar/", "/", "", ["home", "foo", "bar"]),
	split_string("//home/foo   ///   bar/", "/", "/ ", ["home", "foo", "bar"]),
	split_string("   word ", "", " ", ["word"]).
	