:- include(library(s11n)).
:- include(library(canon)).

main :-
	serialize_term(this(is,"a",[123]), X),
%	atom_codes(X, LST),
%	forall(member(Y, LST), put(Y)),
%	nl,
	deserialize_term(X, Z),
	writeq(Z), nl,
	Q = f(Q),
	serialize_term(Q, QS),
%	atom_codes(QS, QLST),
%	forall(member(QY, QLST), put(QY)),
%	nl,
	deserialize_term(QS, QZ),
	\+acyclic_term(QZ),
	canonical_term(QZ, _, P),
	writeq(P), nl.
