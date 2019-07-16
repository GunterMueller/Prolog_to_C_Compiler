%%% Sethi-Ullman register allocation
%
% Expects a tree of 2- or 3-arity structures representing operations
% and produces a list of pseudo-instructions:
%
%   <op>(DEST, SRC1, SRC2)
%   <op>(DEST, SRC)
%   spill(DEST, SRC)
%   load(DEST, SRC)
%
% where SRC/DEST = reg(REG) | tmp(INDEX) | VAL


sethi_ullman_ra(TREE, REGS, TMP0, OUT) :-
	su_ra_label(TREE, r, T),
	su_ra_gencode(T, REGS, TMP0, OUT, [], _).

su_ra_label(T, _, r(L, OP)) :-
	functor(T, N, 2), !,	% binary?
	arg(1, T, A), su_ra_label(A, l, A2), A2 = r(J, _),
	arg(2, T, B), su_ra_label(B, r, B2), B2 = r(K, _),
	(J =:= K -> L is J + 1; L is max(J, K)),
	OP =.. [N, A2, B2].
su_ra_label(T, _, r(J, OP)) :-
	functor(T, N, 1), !,	% unary?
	arg(1, T, A), su_ra_label(A, l, A2), A2 = r(J, _),
	OP =.. [N, A2].
su_ra_label(T, l, r(1, T)).
su_ra_label(T, r, r(0, T)).

su_ra_gencode(r(1, VAL), [R|_], _, [load(reg(R), VAL)|TL], TL, R) :- % left leaf
	atomic(VAL), !.
su_ra_gencode(r(_, OP), REGS, TMP, OUT, TL, R) :- % right child is leaf
	OP =.. [N, A, r(0, VAL)], !,
	su_ra_gencode(A, REGS, TMP, OUT, [OP2|TL], R),
	OP2 =.. [N, reg(R), reg(R), VAL].
su_ra_gencode(r(_, OP), REGS, TMP, OUT, TL, R) :- % single child
	OP =.. [N, A], !,
	su_ra_gencode(A, REGS, TMP, OUT, [OP2|TL], R),
	OP2 =.. [N, reg(R), reg(R)].
su_ra_gencode(r(_, OP), REGS, TMP, OUT, TL, R) :-
	OP =.. [N, r(J, _), r(K, _)],
	length(REGS, RN), 
	su_ra_gencode(OP, J, K, RN, REGS, TMP, OUT, TL, R).

su_ra_gencode(OP, J, K, RN, REGS, TMP, OUT, TL, R1) :-
	K =< J, K > 0, K < RN, !,
	OP =.. [N, A, B],
	su_ra_gencode(A, REGS, TMP, OUT, OUT2, R1),
	select(R1, REGS, REGS2),
	su_ra_gencode(B, REGS2, TMP, OUT2, [OP2|TL], R2),
	OP2 =.. [N, reg(R1), reg(R1), reg(R2)].
su_ra_gencode(OP, J, K, RN, REGS, TMP, OUT, TL, R1) :-
	K > J, J < RN, !,
	OP =.. [N, A, B],
	su_ra_gencode(B, REGS, TMP, OUT, OUT2, R1),
	select(R1, REGS, REGS2),
	su_ra_gencode(A, REGS2, TMP, OUT2, [OP2|TL], R2),
	OP2 =.. [N, reg(R1), reg(R1), reg(R2)].
su_ra_gencode(OP, J, K, RN, REGS, TMP, OUT, TL, R2) :-
	OP =.. [N, A, B],
	su_ra_gencode(B, REGS, TMP, OUT, [spill(tmp(TMP), reg(R1))|OUT2], R1),
	TMP2 is TMP + 1,
	su_ra_gencode(A, REGS, TMP2, OUT2, [OP2|TL], R2),
	OP2 =.. [N, reg(R2), reg(R2), tmp(TMP)].


/*
  
main :-
	sethi_ullman_ra(add(add(sqr(mul(add(u, v), div(x, add(o, 1)))), 10),
			    mul(y, neg(x))),
			[a, b, c], 0, CODE),
	forall(member(X, CODE), (writeq(X),nl)).

*/
