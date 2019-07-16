main :- foo(1).

foo(X) :- Y is X + 1, repeat, bar(123, Y), fail.
bar(123, 2) :- !.
bar(_, _) :- halt(1).

