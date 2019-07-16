main :- foo.
main :- put(66).

foo :-
	bar,
	!,
	fail.
foo :- put(65).

bar :- put(48).
bar :- put(49).
