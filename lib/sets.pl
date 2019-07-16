%%%% set-operations


:- mode union(+, +, -),
	intersection(+, +, -),
	subtract(+, +, -),
	symdiff(+, +, ?),
	symdiff(+, +, ?, ?).

:- determinate symdiff/4.


union([], X, X).
union([X|R], Y, Z):- member(X, Y), !, union(R, Y, Z).
union([X|R], Y, [X|Z]):- union(R, Y, Z).


intersection([], X, []).
intersection([X|R], Y, [X|Z]) :- member(X, Y), !, intersection(R, Y, Z).
intersection([X|R], Y, Z) :- intersection(R, Y, Z).


subtract([], _, []) :- !.
subtract([A|C], B, D) :- member(A, B), !, subtract(C, B, D).
subtract([A|B], C, [A|D]) :- subtract(B, C, D).


select(X, [X|Tail], Tail).
select(Elem, [Head|Tail], [Head|Rest]) :-
	select(Elem, Tail, Rest).


%   symdiff(+Set1, +Set2, ?Diff)
%   is true when Diff is the symmetric difference of Set1 and Set2,
%   that is, if each element of Union occurs in one of Set1 and Set2,
%   but not both.  The construction method is such that the answer
%   will contain no duplicates even if the Sets do.
%
% (this is by Byrd + O'Keefe, SETUTL.PL)

symdiff(Set1, Set2, Diff) :-
	symdiff(Set1, Set2, Diff, Mid),
	symdiff(Set2, Set1, Mid, []).

symdiff([Elem|Rest], Avoid, Diff, Tail) :-
	memberchk(Elem, Avoid), !,
	symdiff(Rest, Avoid, Diff, Tail).
symdiff([Elem|Rest], Avoid, [Elem|Diff], Tail) :- !,
	symdiff(Rest, [Elem|Avoid], Diff, Tail).
symdiff([], _, Tail, Tail).
