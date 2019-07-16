#!/usr/bin/env qp
%% EDCG-preprocessor -*- Prolog -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (C) 1989 Regents of the University of California.
% All rights reserved.  This program may be freely used and modified for
% non-commercial purposes provided this copyright notice is kept unchanged.
% Written by Peter Van Roy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Multiple hidden parameters: an extension to Prolog's DCG notation.
% Version: July 16, 1989

% Comments, suggestions, flames, manifestos, and bug reports are most welcome.
% Please send to:
%	Peter Van Roy
%	508-10 Evans Hall
%	University of California
%	Berkeley, CA 94720
% E-mail: vanroy@bellatrix.berkeley.edu

:- ensure_loaded(library(pp)).

:- op(1200, xfx, ['-->>']).   % Same as ':-'.
:- op( 850, xfx, [':']).      % Slightly tighter than ',' and '\+'.

:- initialization 
	op(1200, xfx, ['-->>']),
	op( 850, xfx, [':']),
	main.


%% transformations

term_expansion((A -->> B), X) :-
        expand_edcg((A -->> B), X).
term_expansion(acc_info(A, B, C, D, E), []) :-
	recordz(edcg_acc_info, acc_info(A, B, C, D, E)).
term_expansion(acc_info(A, B, C, D, E, F, G), []) :-
	recordz(edcg_acc_info, acc_info(A, B, C, D, E, F, G)).
term_expansion(pass_info(A), []) :-
	recordz(edcg_pass_info, pass_info(A)).
term_expansion(pass_info(A, B), []) :-
	recordz(edcg_pass_info, pass_info(A, B)).
term_expansion(pred_info(A, B, C), []) :-
	recordz(edcg_pred_info, pred_info(A, B, C)).


%% edcg-expander

expand_edcg((H-->>B), (TH:-FTB)) :-
	functor(H, Na, Ar),
	'_has_hidden'(H, HList),
	'_new_goal'(H, HList, HArity, TH),
	'_create_acc_pass'(HList, HArity, TH, Acc, Pass),
	'_flat_conj'(B, FB),
	'_expand_body'(FB, TB, Na/Ar, HList, Acc, Pass),
	'_flat_conj'(TB, FTB), !.

'_expand_body'(true, true, _, _, Acc, _) :- '_finish_acc'(Acc).
'_expand_body'((G,B), (TG,TB), NaAr, HList, Acc, Pass) :-
	'_expand_goal'(G, TG, NaAr, HList, Acc, NewAcc, Pass),
	'_expand_body'(B, TB, NaAr, HList, NewAcc, Pass).

% Expand a single goal:
'_expand_goal'({G}, G, _, _, Acc, Acc, _) :- !.
'_expand_goal'(insert(X,Y), LeftA=X, _, _, Acc, NewAcc, _) :-
	'_replace_acc'(dcg, LeftA, RightA, Y, RightA, Acc, NewAcc), !.
'_expand_goal'(insert(X,Y):A, LeftA=X, _, _, Acc, NewAcc, _) :-
	'_replace_acc'(A, LeftA, RightA, Y, RightA, Acc, NewAcc), !.
% Force hidden arguments in L to be appended to G:
'_expand_goal'((G:A), TG, _, HList, Acc, NewAcc, Pass) :-
	\+'_list'(G),
	'_has_hidden'(G, []), !,
	'_make_list'(A, AList),
	'_new_goal'(G, AList, GArity, TG),
	'_use_acc_pass'(AList, GArity, TG, Acc, NewAcc, Pass).
% Use G's regular hidden arguments & override defaults for those arguments
% not in the head:
'_expand_goal'((G:A), TG, _, HList, Acc, NewAcc, Pass) :-
	\+'_list'(G),
	'_has_hidden'(G, GList), GList\==[], !,
	'_make_list'(A, L),
	'_new_goal'(G, GList, GArity, TG),
	'_replace_defaults'(GList, NGList, L),
	'_use_acc_pass'(NGList, GArity, TG, Acc, NewAcc, Pass).
'_expand_goal'((L:A), Joiner, NaAr, _, Acc, NewAcc, _) :-
	'_list'(L), !,
	'_joiner'(L, A, NaAr, Joiner, Acc, NewAcc).
'_expand_goal'(L, Joiner, NaAr, _, Acc, NewAcc, _) :-
	'_list'(L), !,
	'_joiner'(L, dcg, NaAr, Joiner, Acc, NewAcc).
'_expand_goal'((X/A), true, _, _, Acc, Acc, _) :-
	var(X), nonvar(A),
	member(acc(A,X,_), Acc), !.
'_expand_goal'((X/A), true, _, _, Acc, Acc, Pass) :-
	var(X), nonvar(A),
	member(pass(A,X), Pass), !.
'_expand_goal'((A/X), true, _, _, Acc, Acc, _) :-
	var(X), nonvar(A),
	member(acc(A,_,X), Acc), !.
'_expand_goal'((X/A/Y), true, _, _, Acc, Acc, _) :-
	var(X), var(Y), nonvar(A),
	member(acc(A,X,Y), Acc), !.
'_expand_goal'((X/Y), true, NaAr, _, Acc, Acc, _) :-
	write('*** Warning: in '),write(NaAr),write(' the term '),write(X/Y),
	write(' uses a non-existent hidden parameter.'),nl.
% Defaulty cases:
'_expand_goal'(G, TG, HList, _, Acc, NewAcc, Pass) :-
	'_has_hidden'(G, GList), !,
	'_new_goal'(G, GList, GArity, TG),
	'_use_acc_pass'(GList, GArity, TG, Acc, NewAcc, Pass).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Operations on the Acc and Pass data structures:

% Create the Acc and Pass data structures:
% Acc contains terms of the form acc(A,LeftA,RightA) where A is the name of an
% accumulator, and RightA and LeftA are the accumulating parameters.
% Pass contains terms of the form pass(A,Arg) where A is the name of a passed
% argument, and Arg is the argument.
'_create_acc_pass'([], _, _, [], []).
'_create_acc_pass'([A|AList], Index, TGoal, [acc(A,LeftA,RightA)|Acc], Pass) :-
	'_is_acc'(A), !,
	Index1 is Index+1,
	arg(Index1, TGoal, LeftA),
	Index2 is Index+2,
	arg(Index2, TGoal, RightA),
	'_create_acc_pass'(AList, Index2, TGoal, Acc, Pass).
'_create_acc_pass'([A|AList], Index, TGoal, Acc, [pass(A,Arg)|Pass]) :-
	'_is_pass'(A), !,
	Index1 is Index+1,
	arg(Index1, TGoal, Arg),
	'_create_acc_pass'(AList, Index1, TGoal, Acc, Pass).
'_create_acc_pass'([A|AList], Index, TGoal, Acc, Pass) :-
	\+'_is_acc'(A),
	\+'_is_pass'(A),
	write('*** Error: '),write(A),
	write(' is not a hidden parameter.'),nl.

% Use the Acc and Pass data structures to create the arguments of a body goal:
% Add the hidden parameters named in GList to the goal.
'_use_acc_pass'([], _, _, Acc, Acc, _).
% 1a. The accumulator A is used in the head:
'_use_acc_pass'([A|GList], Index, TGoal, Acc, NewAcc, Pass) :-
	'_replace_acc'(A, LeftA, RightA, MidA, RightA, Acc, MidAcc), !,
	Index1 is Index+1,
	arg(Index1, TGoal, LeftA),
	Index2 is Index+2,
	arg(Index2, TGoal, MidA),
	'_use_acc_pass'(GList, Index2, TGoal, MidAcc, NewAcc, Pass).
% 1b. The accumulator A is not used in the head:
'_use_acc_pass'([A|GList], Index, TGoal, Acc, NewAcc, Pass) :-
	'_acc_info'(A, LStart, RStart), !,
	Index1 is Index+1,
	arg(Index1, TGoal, LStart),
	Index2 is Index+2,
	arg(Index2, TGoal, RStart),
	'_use_acc_pass'(GList, Index2, TGoal, Acc, NewAcc, Pass).
% 2a. The passed argument A is used in the head:
'_use_acc_pass'([A|GList], Index, TGoal, Acc, NewAcc, Pass) :-
	'_is_pass'(A),
	member(pass(A,Arg), Pass), !,
	Index1 is Index+1,
	arg(Index1, TGoal, Arg),
	'_use_acc_pass'(GList, Index1, TGoal, Acc, NewAcc, Pass).
% 2b. The passed argument A is not used in the head:
'_use_acc_pass'([A|GList], Index, TGoal, Acc, NewAcc, Pass) :-
	'_pass_info'(A, AStart), !,
	Index1 is Index+1,
	arg(Index1, TGoal, AStart),
	'_use_acc_pass'(GList, Index1, TGoal, Acc, NewAcc, Pass).
% 3. Defaulty case when A does not exist:
'_use_acc_pass'([A|GList], Index, TGoal, Acc, Acc, Pass) :-
	write('*** Error: the hidden parameter '),write(A),
	write(' does not exist.'),nl.

% Finish the Acc data structure:
% Link its Left and Right accumulation variables together in pairs:
'_finish_acc'([]).
'_finish_acc'([acc(_,Link,Link)|Acc]) :- '_finish_acc'(Acc).

% Replace elements in the Acc data structure:
% Succeeds iff replacement is successful.
'_replace_acc'(A, L1, R1, L2, R2, Acc, NewAcc) :-
	member(acc(A,L1,R1), Acc), !,
	'_replace'(acc(A,_,_), acc(A,L2,R2), Acc, NewAcc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Specialized utilities:

% Given a goal Goal and a list of hidden parameters GList 
% create a new goal TGoal with the correct number of arguments.
% Also return the arity of the original goal.
'_new_goal'(Goal, GList, GArity, TGoal) :-
	functor(Goal, Name, GArity),
	'_number_args'(GList, GArity, TArity),
	functor(TGoal, Name, TArity),
	'_match'(1, GArity, Goal, TGoal).

% Add the number of arguments needed for the hidden parameters:
'_number_args'([], N, N).
'_number_args'([A|List], N, M) :-
	'_is_acc'(A), !,
	N2 is N+2,
	'_number_args'(List, N2, M).
'_number_args'([A|List], N, M) :-
	'_is_pass'(A), !,
	N1 is N+1,
	'_number_args'(List, N1, M).

% Give a list of G's hidden parameters:
'_has_hidden'(G, GList) :-
	functor(G, GName, GArity),
	recorded(edcg_pred_info, pred_info(GName, GArity, GList)).
'_has_hidden'(G, []) :-
	functor(G, GName, GArity), 
	\+recorded(edcg_pred_info, pred_info(GName, GArity, _)).

% Succeeds if A is an accumulator:
'_is_acc'(A)  :- atomic(A), !, '_acc_info'(A, _, _, _, _, _, _).
'_is_acc'(A)  :- functor(A, N, 2), !, '_acc_info'(N, _, _, _, _, _, _).

% Succeeds if A is a passed argument:
'_is_pass'(A) :- atomic(A), !, '_pass_info'(A, _).
'_is_pass'(A) :- functor(A, N, 1), !, '_pass_info'(N, _).

% Get initial values for the accumulator:
'_acc_info'(AccParams, LStart, RStart) :-
	functor(AccParams, Acc, 2),
	'_is_acc'(Acc), !,
	arg(1, AccParams, LStart),
	arg(2, AccParams, RStart).
'_acc_info'(Acc, LStart, RStart) :-
	'_acc_info'(Acc, _, _, _, _, LStart, RStart).

% Isolate the internal database from the user database:
'_acc_info'(Acc, Term, Left, Right, Joiner, LStart, RStart) :-
	recorded(edcg_acc_info, acc_info(Acc, Term, Left, Right, Joiner, LStart, RStart)).
'_acc_info'(Acc, Term, Left, Right, Joiner, _, _) :-
	recorded(edcg_acc_info, acc_info(Acc, Term, Left, Right, Joiner)).
'_acc_info'(dcg, Term, Left, Right, Left=[Term|Right], _, []).

% Get initial value for the passed argument:
% Also, isolate the internal database from the user database.
'_pass_info'(PassParam, PStart) :-
	functor(PassParam, Pass, 1),
	'_is_pass'(Pass), !,
	arg(1, PassParam, PStart).
'_pass_info'(Pass, PStart) :-
	recorded(edcg_pass_info, pass_info(Pass, PStart)).
'_pass_info'(Pass, _) :-
	recorded(edcg_pass_info, pass_info(Pass)).

% Calculate the joiner for an accumulator A:
'_joiner'([], _, _, true, Acc, Acc).
'_joiner'([Term|List], A, NaAr, (Joiner,LJoiner), Acc, NewAcc) :-
	'_replace_acc'(A, LeftA, RightA, MidA, RightA, Acc, MidAcc),
	'_acc_info'(A, Term, LeftA, MidA, Joiner, _, _), !,
	'_joiner'(List, A, NaAr, LJoiner, MidAcc, NewAcc).
% Defaulty case:
'_joiner'([Term|List], A, NaAr, Joiner, Acc, NewAcc) :-
	write('*** Warning: in '),write(NaAr),
	write(' the accumulator '),write(A),
	write(' does not exist.'),nl,
	'_joiner'(List, A, NaAr, Joiner, Acc, NewAcc).

% Replace hidden parameters with ones containing initial values:
'_replace_defaults'([], [], _).
'_replace_defaults'([A|GList], [NA|NGList], AList) :-
	'_replace_default'(A, NA, AList),
	'_replace_defaults'(GList, NGList, AList).

'_replace_default'(A, NewA, AList) :-  % New initial values for accumulator.
	functor(NewA, A, 2),
	member(NewA, AList), !.
'_replace_default'(A, NewA, AList) :-  % New initial values for passed argument.
	functor(NewA, A, 1),
	member(NewA, AList), !.
'_replace_default'(A, NewA, _) :-      % Use default initial values.
	A=NewA.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Generic utilities:

% Match arguments L, L+1, ..., H of the predicates P and Q:
'_match'(L, H, _, _) :- L>H, !.
'_match'(L, H, P, Q) :- L=<H, !,
	arg(L, P, A),
	arg(L, Q, A),
	L1 is L+1,
	'_match'(L1, H, P, Q).

% Flatten a conjunction and terminate it with 'true':
'_flat_conj'(Conj, FConj) :- '_flat_conj'(Conj, FConj, true).

'_flat_conj'(true, X, X).
'_flat_conj'((A,B), X1, X3) :-
	'_flat_conj'(A, X1, X2),
	'_flat_conj'(B, X2, X3).
'_flat_conj'(G, (G,X), X) :-
	\+G=true,
	\+G=(_,_).

'_list'(L) :- nonvar(L), L=[_|_], !.
'_list'(L) :- L==[], !.

'_make_list'(A, [A]) :- \+'_list'(A), !.
'_make_list'(L,   L) :-   '_list'(L), !.

% replace(Elem, RepElem, List, RepList)
'_replace'(_, _, [], []).
'_replace'(A, B, [A|L], [B|R]) :- !,
	'_replace'(A, B, L, R).
'_replace'(A, B, [C|L], [C|R]) :-
	\+C=A, !,
	'_replace'(A, B, L, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
