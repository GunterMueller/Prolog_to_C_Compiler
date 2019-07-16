%%%% s11n - prolog part

:- verbatim('#include "s11n.c"').
:- determinate serialize_term/2, deserialize_term/2.


serialize_term(X, S) :-
	\+integer(X), X \= [],		% needs to have a location
	foreign_call(serialize(X, S)).

deserialize_term(S, X) :-
	foreign_call(deserialize(S, X)).
