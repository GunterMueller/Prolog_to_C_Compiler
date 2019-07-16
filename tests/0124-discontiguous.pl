:- discontiguous foo/1, bar/0, bar/1.

foo(1).
bar :- display(bar1), nl.

main :- foo(X), write(X), nl, fail.
main :- bar, bar(123).

foo(2).
bar(X) :- display(X), nl.
foo(3).
