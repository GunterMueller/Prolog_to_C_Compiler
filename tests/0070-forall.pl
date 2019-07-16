main :-
	forall(member(X,[1,2,3]), display(X)), nl, fail.
main :-
	forall(member(X,[1,2,3]), fail), display(nope).
main.
