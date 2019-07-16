%%% operator definitions


:- mode op(+, +, +),
	current_op(+, ?, ?, +).

:- global_variable(operator_table).
:- pre_initialization(global_set(operator_table, [op(700,xfx,'@=<'),
						  op(1100,xfy,';'),
						  op(700,xfx,'@<'),
						  op(1000,xfy,','),
						  op(700,xfx,'is'),
						  op(200,xfx,'**'),
						  op(700,xfx,'@>'),
						  op(400,yfx,'<<'),
						  op(700,xfx,'@>='),
						  op(700,xfx,'\\=='),
						  op(200,xfy,'^'),
						  op(200,fy,'+'),
						  op(500,yfx,'+'),
						  op(400,yfx,'>>'),
						  op(700,xfx,'=='),
						  op(200,fy,'-'),
						  op(500,yfx,'-'),
						  op(1150,fx,('initialization')),
						  op(1150,fx,('public')),
						  op(1150,fx,('meta_predicate')),
						  op(1150,fx,('discontiguous')),
						  op(1150,fx,('determinate')),
						  op(1150,fx,('mode')),
						  op(900,fy,'\\+'),
						  op(700,xfx,'=:='),
						  op(700,xfx,'=\\='),
						  op(500,yfx,'\\/'),
						  op(1200,fx,(':-')),
						  op(1200,xfx,(':-')),
						  op(700,xfx,'\\='),
						  op(200,fy,'\\'),
						  op(500,yfx,'/\\'),
						  op(400,yfx,'xor'),
						  op(700,xfx,'=<'),
						  op(700,xfx,'<'),
						  op(700,xfx,'=..'),
						  op(700,xfx,'='),
						  op(400,yfx,'//'),
						  op(400,yfx,'/'),
						  op(1050,xfy,'->'),
						  op(700,xfx,'>'),
						  op(400,yfx,'*'),
						  op(700,xfx,'>='),
						  op(1200,xfx,'-->'),
						  op(400,yfx,'\\\\'),
						  op(400,yfx,'rem')])).

:- determinate '$op'/3.						  

						  
current_op(P, A, N) :-
	global_ref(operator_table, OT),
	( var(N)
	-> member(op(P, A, N), OT)
	; foreign_call(fast_assq(N, OT, 3, FOUND)),
	  current_op(FOUND, P, A, N)
	).
current_op([op(P, A, N)|_], P, A, N).
current_op([_|MORE], P, A, N) :-
	foreign_call(fast_assq(N, MORE, 3, FOUND)),
	current_op(FOUND, P, A, N).

op(P, A, []) :- !.
op(P, A, [N|R]) :-
	!, op(P, A, N), op(P, A, R).
op(P, A, N) :-
	(P \== ','; throw(error('operator \',\' may not be redefined'))),
	(integer(P), P >= 0, P =< 1200; throw(error('bad operator precedence', P))),
	(memberchk(A, [xf, yf, xfx, xfy, yfx, fx, fy]); throw(error('bad operator associativity', A))),
	!,
	'$op'(P, A, N).

'$op'(P, A, N) :-
	global_ref(operator_table, OT),
	(select(op(_, _, N), OT, OT2); OT2 = OT),
	!,
	copy_term([op(P, A, N)|OT2], OT3),
	global_set(operator_table, OT3).
