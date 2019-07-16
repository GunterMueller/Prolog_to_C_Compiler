:- include(library(pp)).

:- op(1100, xfx, '=>').
:- initialization op(1100, xfx, '=>'), main1.


term_expansion((X => Y), (rule :- (X) -> Y)).
term_expansion((X + Y), [declare(X/Y), add(X, Y)]).

goal_expansion(-X, \+X).

main1 :-
	see('tests/pp-sample.in'),
	main.
