main :- foo, bar.

foo :-
	display('foo1'), nl.
foo :-
	display('foo2'), nl.

bar :-
	display('bar1'), nl,
	fail.
bar :-
	display('bar2'), nl,
	fail.
