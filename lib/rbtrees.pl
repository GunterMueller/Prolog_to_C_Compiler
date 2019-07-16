/*

	This code implements Red-Black trees as described in:

	"Introduction to Algorithms", Second Edition
	Cormen, Leiserson, Rivest, and Stein,
	MIT Press

        Author: Vitor Santos Costa

*/


/*
:- module(rbtrees,
	  [rb_new/1,
	   rb_empty/1,		% ?T
	   rb_lookup/3,		% +Key, -Value, +T
	   rb_update/4,		% +T, +Key, +NewVal, -TN
	   rb_update/5,		% +T, +Key, ?OldVal, +NewVal, -TN
	   rb_lookupall/3       % +Key, -Value, +T
	   rb_insert/4,		% +T0, +Key, ?Value, -TN
	   rb_insert_new/4,	% +T0, +Key, ?Value, -TN
	   rb_delete/3,		% +T, +Key, -TN
	   rb_delete/4,		% +T, +Key, -Val, -TN
	   rb_visit/2,			% +T, -Pairs
	   rb_visit/3,
	   rb_keys/2,			% +T, +Keys
	   rb_keys/3,
	   rb_clone/3,
	   rb_clone/4,
	   rb_min/3,
	   rb_max/3,
	   rb_del_min/4,
	   rb_del_max/4,
	   rb_next/4,
	   rb_previous/4,
	   list_to_rbtree/2,
	   ord_list_to_rbtree/2,
	   is_rbtree/1,
	   rb_size/2,
	   rb_in/3
       ]).
*/

/** <module> Red black trees

Red-Black trees are balanced search binary trees. They are named because
nodes can be classified as either red or   black. The code we include is
based on "Introduction  to  Algorithms",   second  edition,  by  Cormen,
Leiserson, Rivest and Stein. The library   includes  routines to insert,
lookup and delete elements in the tree.

A Red black tree is represented as a term t(Nil, Tree), where Nil is the
Nil-node, a node shared for each nil-node in  the tree. Any node has the
form colour(Left, Key, Value, Right), where _colour_  is one of =red= or
=black=.

@author Vitor Santos Costa, Jan Wielemaker
*/

% create an empty tree.
%%	rb_new(-T) is det.
%
%	Create a new Red-Black tree.
%
%	@deprecated	Use rb_empty/1.

rb_new(t(Nil,Nil)) :- Nil = black('',_,_,'').

%%	rb_empty(?T) is semidet.
%
%	Succeeds if T is an empty Red-Black tree.

rb_empty(t(Nil,Nil)) :- Nil = black('',_,_,'').

%%	rb_lookup(+Key, -Value, +T) is semidet.
%
%	Backtrack through all elements with  key   Key  in the Red-Black
%	tree T, returning for each the value Value.

rb_lookup(Key, Val, t(_,Tree)) :-
	'$rb_lookup'(Key, Val, Tree).

'$rb_lookup'(_, _, black('',_,_,'')) :- !, fail.
'$rb_lookup'(Key, Val, Tree) :-
	arg(2,Tree,KA),
	compare(Cmp,KA,Key),
	'$rb_lookup'(Cmp,Key,Val,Tree).

'$rb_lookup'(>, K, V, Tree) :-
	arg(1,Tree,NTree),
	'$rb_lookup'(K, V, NTree).
'$rb_lookup'(<, K, V, Tree) :-
	arg(4,Tree,NTree),
	'$rb_lookup'(K, V, NTree).
'$rb_lookup'(=, _, V, Tree) :-
	arg(3,Tree,V).

%%	rb_min(+T, -Key, -Value) is semidet.
%
%	Key is the minimum key in T, and is associated with Val.

rb_min(t(_,Tree), Key, Val) :-
	'$rb_min'(Tree, Key, Val).

'$rb_min'(red(black('',_,_,_),Key,Val,_), Key, Val) :- !.
'$rb_min'(black(black('',_,_,_),Key,Val,_), Key, Val) :- !.
'$rb_min'(red(Right,_,_,_), Key, Val) :-
	'$rb_min'(Right,Key,Val).
'$rb_min'(black(Right,_,_,_), Key, Val) :-
	'$rb_min'(Right,Key,Val).

%%	rb_max(+T, -Key, -Value) is semidet.
%
%	Key is the maximal key in T, and is associated with Val.

rb_max(t(_,Tree), Key, Val) :-
	'$rb_max'(Tree, Key, Val).

'$rb_max'(red(_,Key,Val,black('',_,_,_)), Key, Val) :- !.
'$rb_max'(black(_,Key,Val,black('',_,_,_)), Key, Val) :- !.
'$rb_max'(red(_,_,_,Left), Key, Val) :-
	'$rb_max'(Left,Key,Val).
'$rb_max'(black(_,_,_,Left), Key, Val) :-
	'$rb_max'(Left,Key,Val).

%%	rb_next(+T, +Key, -Next,-Value) is semidet.
%
%	Next is the next element after Key  in T, and is associated with
%	Val.

rb_next(t(_,Tree), Key, Next, Val) :-
	'$rb_next'(Tree, Key, Next, Val, []).

'$rb_next'(black('',_,_,''), _, _, _, _) :- !, fail.
'$rb_next'(Tree, Key, Next, Val, Candidate) :-
	arg(2,Tree,KA),
	arg(3,Tree,VA),
	compare(Cmp,KA,Key),
	'$rb_next'(Cmp, Key, KA, VA, Next, Val, Tree, Candidate).

'$rb_next'(>, K, KA, VA, NK, V, Tree, _) :-
	arg(1,Tree,NTree),
	'$rb_next'(NTree,K,NK,V,KA-VA).
'$rb_next'(<, K, _, _, NK, V, Tree, Candidate) :-
	arg(4,Tree,NTree),
	'$rb_next'(NTree,K,NK,V,Candidate).
'$rb_next'(=, _, _, _, NK, Val, Tree, Candidate) :-
	arg(4,Tree,NTree),
	(
	    '$rb_min'(NTree, NK, Val)
	-> true
	;
	    Candidate = (NK-Val)
	).

%%	rb_previous(+T, +Key, -Previous, -Value) is semidet.
%
%	Previous is the  previous  element  after   Key  in  T,  and  is
%	associated with Val.

rb_previous(t(_,Tree), Key, Previous, Val) :-
	'$rb_previous'(Tree, Key, Previous, Val, []).

'$rb_previous'(black('',_,_,''), _, _, _, _) :- !, fail.
'$rb_previous'(Tree, Key, Previous, Val, Candidate) :-
	arg(2,Tree,KA),
	arg(3,Tree,VA),
	compare(Cmp,KA,Key),
	'$rb_previous'(Cmp, Key, KA, VA, Previous, Val, Tree, Candidate).

'$rb_previous'(>, K, _, _, NK, V, Tree, Candidate) :-
	arg(1,Tree,NTree),
	'$rb_previous'(NTree,K,NK,V,Candidate).
'$rb_previous'(<, K, KA, VA, NK, V, Tree, _) :-
	arg(4,Tree,NTree),
	'$rb_previous'(NTree,K,NK,V,KA-VA).
'$rb_previous'(=, _, _, _, K, Val, Tree, Candidate) :-
	arg(1,Tree,NTree),
	(
	    '$rb_max'(NTree, K, Val)
	-> true
	;
	    Candidate = (K-Val)
	).

%%	rb_update(+T, +Key, +NewVal, -TN) is semidet.
%%	rb_update(+T, +Key, ?OldVal, +NewVal, -TN) is semidet.
%
%	Tree TN is tree T,  but  with   value  for  Key  associated with
%	NewVal.  Fails if it cannot find Key in T.

rb_update(t(Nil,OldTree), Key, OldVal, Val, t(Nil,NewTree)) :-
	'$rb_update'(OldTree, Key, OldVal, Val, NewTree).

rb_update(t(Nil,OldTree), Key, Val, t(Nil,NewTree)) :-
	'$rb_update'(OldTree, Key, _, Val, NewTree).

'$rb_update'(black(Left,Key0,Val0,Right), Key, OldVal, Val, NewTree) :-
	Left \= [],
	compare(Cmp,Key0,Key),
	(Cmp == (=)
	-> OldVal = Val0,
	    NewTree = black(Left,Key0,Val,Right)
	;
	Cmp == (>) ->
	   NewTree = black(NewLeft,Key0,Val0,Right),
	  '$rb_update'(Left, Key, OldVal, Val, NewLeft)
	;
	  NewTree = black(Left,Key0,Val0,NewRight),
	  '$rb_update'(Right, Key, OldVal, Val, NewRight)
	).
'$rb_update'(red(Left,Key0,Val0,Right), Key, OldVal, Val, NewTree) :-
	compare(Cmp,Key0,Key),
	(Cmp == (=)
	-> OldVal = Val0,
	    NewTree = red(Left,Key0,Val,Right)
	;
	Cmp == (>)
	-> NewTree = red(NewLeft,Key0,Val0,Right),
	  '$rb_update'(Left, Key, OldVal, Val, NewLeft)
	;
	  NewTree = red(Left,Key0,Val0,NewRight),
	  '$rb_update'(Right, Key, OldVal, Val, NewRight)
	).

%%	rb_in(?Key, ?Val, +Tree) is nondet.
%
%	True if Key-Val appear in Tree. Uses indexing if Key is bound.

rb_in(Key, Val, t(_,T)) :-
	var(Key), !,
	'$rb_enum'(Key, Val, T).
rb_in(Key, Val, t(_,T)) :-
	'$rb_lookup'(Key, Val, T).


'$rb_enum'(Key, Val, black(L,K,V,R)) :-
	L \= '',
	'$rb_enum_cases'(Key, Val, L, K, V, R).
'$rb_enum'(Key, Val, red(L,K,V,R)) :-
	'$rb_enum_cases'(Key, Val, L, K, V, R).

'$rb_enum_cases'(Key, Val, L, _, _, _) :-
	'$rb_enum'(Key, Val, L).
'$rb_enum_cases'(Key, Val, _, Key, Val, _).
'$rb_enum_cases'(Key, Val, _, _, _, R) :-
	'$rb_enum'(Key, Val, R).


%%	rb_lookupall(+Key, -Value, +T)
%
%	Lookup all elements with  key  Key   in  the  red-black  tree T,
%	returning the value Value.

rb_lookupall(Key, Val, t(_,Tree)) :-
	'$rb_lookupall'(Key, Val, Tree).


'$rb_lookupall'(_, _, black('',_,_,'')) :- !, fail.
'$rb_lookupall'(Key, Val, Tree) :-
	arg(2,Tree,KA),
	compare(Cmp,KA,Key),
	'$rb_lookupall'(Cmp,Key,Val,Tree).

'$rb_lookupall'(>, K, V, Tree) :-
	arg(4,Tree,NTree),
	rb_lookupall(K, V, NTree).
'$rb_lookupall'(=, _, V, Tree) :-
	arg(3,Tree,V).
'$rb_lookupall'(=, K, V, Tree) :-
	arg(1,Tree,NTree),
	'$rb_lookupall'(K, V, NTree).
'$rb_lookupall'(<, K, V, Tree) :-
	arg(1,Tree,NTree),
	'$rb_lookupall'(K, V, NTree).

		 /*******************************
		 *	 TREE INSERTION		*
		 *******************************/

% We don't use parent nodes, so we may have to fix the root.

%%	rb_insert(+T0, +Key, ?Value, -TN) is det.
%
%	Add an element with key Key and Value  to the tree T0 creating a
%	new red-black tree TN. If Key  is   a  key in T0, the associated
%	value is replaced by Value.  See also rb_insert_new/4.

rb_insert(t(Nil,Tree0),Key,Val,t(Nil,Tree)) :-
	'$rb_insert'(Tree0,Key,Val,Nil,Tree).


'$rb_insert'(Tree0,Key,Val,Nil,Tree) :-
	'$rb_insert2'(Tree0,Key,Val,Nil,TreeI,_),
	'$rb_fix_root'(TreeI,Tree).

%
% Cormen et al present the algorithm as
% (1) standard tree insertion;
% (2) from the viewpoint of the newly inserted node:
%     partially fix the tree;
%     move upwards
% until reaching the root.
%
% We do it a little bit different:
%
% (1) standard tree insertion;
% (2) move upwards:
%      when reaching a black node;
%        if the tree below may be broken, fix it.
% We take advantage of Prolog unification
% to do several operations in a single go.
%



%
% actual insertion
%
'$rb_insert2'(black('',_,_,''), K, V, Nil, T, Status) :- !,
	T = red(Nil,K,V,Nil),
	Status = not_done.
'$rb_insert2'(red(L,K0,V0,R), K, V, Nil, NT, Flag) :-
	( K @< K0
	-> NT = red(NL,K0,V0,R),
	  '$rb_insert2'(L, K, V, Nil, NL, Flag)
	; K == K0 ->
	  NT = red(L,K0,V,R),
	  Flag = done
	;
	  NT = red(L,K0,V0,NR),
	  '$rb_insert2'(R, K, V, Nil, NR, Flag)
	).
'$rb_insert2'(black(L,K0,V0,R), K, V, Nil, NT, Flag) :-
	( K @< K0
	-> '$rb_insert2'(L, K, V, Nil, IL, Flag0),
	  '$rb_fix_left'(Flag0, black(IL,K0,V0,R), NT, Flag)
	; K == K0 ->
	  NT =	black(L,K0,V,R),
	  Flag = done
	;
	  '$rb_insert2'(R, K, V, Nil, IR, Flag0),
	  '$rb_fix_right'(Flag0, black(L,K0,V0,IR), NT, Flag)
	).

% We don't use parent nodes, so we may have to fix the root.

%%	rb_insert_new(+T0, +Key, ?Value, -TN) is semidet.
%
%	Add a new element with key Key and Value  to the tree T0 creating a
%	new red-black tree TN.   Fails if Key is a key in T0.

rb_insert_new(t(Nil,Tree0),Key,Val,t(Nil,Tree)) :-
	'$rb_insert_new'(Tree0,Key,Val,Nil,Tree).


'$rb_insert_new'(Tree0,Key,Val,Nil,Tree) :-
	'$rb_insert_new_2'(Tree0,Key,Val,Nil,TreeI,_),
	'$rb_fix_root'(TreeI,Tree).

%
% actual insertion, copied from insert2
%
'$rb_insert_new_2'(black('',_,_,''), K, V, Nil, T, Status) :- !,
	T = red(Nil,K,V,Nil),
	Status = not_done.
'$rb_insert_new_2'(red(L,K0,V0,R), K, V, Nil, NT, Flag) :-
	( K @< K0
	-> NT = red(NL,K0,V0,R),
	  '$rb_insert_new_2'(L, K, V, Nil, NL, Flag)
	; K == K0 ->
	  fail
	;
	  NT = red(L,K0,V0,NR),
	  '$rb_insert_new_2'(R, K, V, Nil, NR, Flag)
	).
'$rb_insert_new_2'(black(L,K0,V0,R), K, V, Nil, NT, Flag) :-
	( K @< K0
	-> '$rb_insert_new_2'(L, K, V, Nil, IL, Flag0),
	  '$rb_fix_left'(Flag0, black(IL,K0,V0,R), NT, Flag)
	; K == K0 ->
	  fail
	;
	  '$rb_insert_new_2'(R, K, V, Nil, IR, Flag0),
	  '$rb_fix_right'(Flag0, black(L,K0,V0,IR), NT, Flag)
	).

%
% make sure the root is always black.
%
'$rb_fix_root'(black(L,K,V,R),black(L,K,V,R)).
'$rb_fix_root'(red(L,K,V,R),black(L,K,V,R)).



%
% How to fix if we have inserted on the left
%
'$rb_fix_left'(done,T,T,done) :- !.
'$rb_fix_left'(not_done,Tmp,Final,Done) :-
	'$rb_fix_left'(Tmp,Final,Done).

%
% case 1 of RB: just need to change colors.
%
'$rb_fix_left'(black(red(Al,AK,AV,red(Be,BK,BV,Ga)),KC,VC,red(De,KD,VD,Ep)),
	red(black(Al,AK,AV,red(Be,BK,BV,Ga)),KC,VC,black(De,KD,VD,Ep)),
	not_done) :- !.
'$rb_fix_left'(black(red(red(Al,KA,VA,Be),KB,VB,Ga),KC,VC,red(De,KD,VD,Ep)),
	red(black(red(Al,KA,VA,Be),KB,VB,Ga),KC,VC,black(De,KD,VD,Ep)),
	not_done) :- !.
%
% case 2 of RB: got a knee so need to do rotations
%
'$rb_fix_left'(black(red(Al,KA,VA,red(Be,KB,VB,Ga)),KC,VC,De),
	black(red(Al,KA,VA,Be),KB,VB,red(Ga,KC,VC,De)),
	done) :- !.
%
% case 3 of RB: got a line
%
'$rb_fix_left'(black(red(red(Al,KA,VA,Be),KB,VB,Ga),KC,VC,De),
	black(red(Al,KA,VA,Be),KB,VB,red(Ga,KC,VC,De)),
	done) :- !.
%
% case 4 of RB: nothing to do
%
'$rb_fix_left'(T,T,done).

%
% How to fix if we have inserted on the right
%
'$rb_fix_right'(done,T,T,done) :- !.
'$rb_fix_right'(not_done,Tmp,Final,Done) :-
	'$rb_fix_right'(Tmp,Final,Done).

%
% case 1 of RB: just need to change colors.
%
'$rb_fix_right'(black(red(Ep,KD,VD,De),KC,VC,red(red(Ga,KB,VB,Be),KA,VA,Al)),
	red(black(Ep,KD,VD,De),KC,VC,black(red(Ga,KB,VB,Be),KA,VA,Al)),
	not_done) :- !.
'$rb_fix_right'(black(red(Ep,KD,VD,De),KC,VC,red(Ga,Ka,Va,red(Be,KB,VB,Al))),
	red(black(Ep,KD,VD,De),KC,VC,black(Ga,Ka,Va,red(Be,KB,VB,Al))),
	not_done) :- !.
%
% case 2 of RB: got a knee so need to do rotations
%
'$rb_fix_right'(black(De,KC,VC,red(red(Ga,KB,VB,Be),KA,VA,Al)),
	black(red(De,KC,VC,Ga),KB,VB,red(Be,KA,VA,Al)),
	done) :- !.
%
% case 3 of RB: got a line
%
'$rb_fix_right'(black(De,KC,VC,red(Ga,KB,VB,red(Be,KA,VA,Al))),
	black(red(De,KC,VC,Ga),KB,VB,red(Be,KA,VA,Al)),
	done) :- !.
%
% case 4 of RB: nothing to do.
%
'$rb_fix_right'(T,T,done).


rb_delete(t(Nil,T), K, t(Nil,NT)) :-
	'$rb_delete'(T, K, _, NT, _).

%%	rb_delete(+T, +Key, -TN).
%%	rb_delete(+T, +Key, -Val, -TN).
%
%	Delete element with key Key from the tree T, returning the value
%	Val associated with the key and a new tree TN.

rb_delete(t(Nil,T), K, V, t(Nil,NT)) :-
	'$rb_delete'(T, K, V0, NT, _),
	V = V0.

%
% I am afraid our representation is not as nice for delete
%
'$rb_delete'(red(L,K0,V0,R), K, V, NT, Flag) :-
	K @< K0, !,
	'$rb_delete'(L, K, V, NL, Flag0),
	'$rb_fixup_left'(Flag0,red(NL,K0,V0,R),NT, Flag).
'$rb_delete'(red(L,K0,V0,R), K, V, NT, Flag) :-
	K @> K0, !,
	'$rb_delete'(R, K, V, NR, Flag0),
	'$rb_fixup_right'(Flag0,red(L,K0,V0,NR),NT, Flag).
'$rb_delete'(red(L,_,V,R), _, V, OUT, Flag) :-
%	K == K0,
	'$rb_delete_red_node'(L,R,OUT,Flag).
'$rb_delete'(black(L,K0,V0,R), K, V, NT, Flag) :-
	K @< K0, !,
	'$rb_delete'(L, K, V, NL, Flag0),
	'$rb_fixup_left'(Flag0,black(NL,K0,V0,R),NT, Flag).
'$rb_delete'(black(L,K0,V0,R), K, V, NT, Flag) :-
	K @> K0, !,
	'$rb_delete'(R, K, V, NR, Flag0),
	'$rb_fixup_right'(Flag0,black(L,K0,V0,NR),NT, Flag).
'$rb_delete'(black(L,_,V,R), _, V, OUT, Flag) :-
%	K == K0,
	'$rb_delete_black_node'(L,R,OUT,Flag).

%%	rb_del_min(+T, -Key, -Val, -TN)
%
%	Delete the least element from the tree T, returning the key Key,
%	the value Val associated with the key and a new tree TN.

rb_del_min(t(Nil,T), K, Val, t(Nil,NT)) :-
	'$rb_del_min'(T, K, Val, Nil, NT, _).

'$rb_del_min'(red(black('',_,_,_),K,V,R), K, V, Nil, OUT, Flag) :- !,
	'$rb_delete_red_node'(Nil,R,OUT,Flag).
'$rb_del_min'(red(L,K0,V0,R), K, V, Nil, NT, Flag) :-
	'$rb_del_min'(L, K, V, Nil, NL, Flag0),
	'$rb_fixup_left'(Flag0,red(NL,K0,V0,R), NT, Flag).
'$rb_del_min'(black(black('',_,_,_),K,V,R), K, V, Nil, OUT, Flag) :- !,
	'$rb_delete_black_node'(Nil,R,OUT,Flag).
'$rb_del_min'(black(L,K0,V0,R), K, V, Nil, NT, Flag) :-
	'$rb_del_min'(L, K, V, Nil, NL, Flag0),
	'$rb_fixup_left'(Flag0,black(NL,K0,V0,R),NT, Flag).


%%	rb_del_max(+T, -Key, -Val, -TN)
%
%	Delete the largest element from the   tree  T, returning the key
%	Key, the value Val associated with the key and a new tree TN.

rb_del_max(t(Nil,T), K, Val, t(Nil,NT)) :-
	'$rb_del_max'(T, K, Val, Nil, NT, _).

'$rb_del_max'(red(L,K,V,black('',_,_,_)), K, V, Nil, OUT, Flag) :- !,
	'$rb_delete_red_node'(L,Nil,OUT,Flag).
'$rb_del_max'(red(L,K0,V0,R), K, V, Nil, NT, Flag) :-
	'$rb_del_max'(R, K, V, Nil, NR, Flag0),
	'$rb_fixup_right'(Flag0,red(L,K0,V0,NR),NT, Flag).
'$rb_del_max'(black(L,K,V,black('',_,_,_)), K, V, Nil, OUT, Flag) :- !,
	'$rb_delete_black_node'(L,Nil,OUT,Flag).
'$rb_del_max'(black(L,K0,V0,R), K, V, Nil, NT, Flag) :-
	'$rb_del_max'(R, K, V, Nil, NR, Flag0),
	'$rb_fixup_right'(Flag0,black(L,K0,V0,NR), NT, Flag).



'$rb_delete_red_node'(L1,L2,L1,done) :- L1 == L2, !.
'$rb_delete_red_node'(black('',_,_,''),R,R,done) :-  !.
'$rb_delete_red_node'(L,black('',_,_,''),L,done) :-  !.
'$rb_delete_red_node'(L,R,OUT,Done) :-
	'$rb_delete_next'(R,NK,NV,NR,Done0),
	'$rb_fixup_right'(Done0,red(L,NK,NV,NR),OUT,Done).


'$rb_delete_black_node'(L1,L2,L1,not_done) :-		L1 == L2, !.
'$rb_delete_black_node'(black('',_,_,''),red(L,K,V,R),black(L,K,V,R),done) :- !.
'$rb_delete_black_node'(black('',_,_,''),R,R,not_done) :- !.
'$rb_delete_black_node'(red(L,K,V,R),black('',_,_,''),black(L,K,V,R),done) :- !.
'$rb_delete_black_node'(L,black('',_,_,''),L,not_done) :- !.
'$rb_delete_black_node'(L,R,OUT,Done) :-
	'$rb_delete_next'(R,NK,NV,NR,Done0),
	'$rb_fixup_right'(Done0,black(L,NK,NV,NR),OUT,Done).


'$rb_delete_next'(red(black('',_,_,''),K,V,R),K,V,R,done) :-	!.
'$rb_delete_next'(black(black('',_,_,''),K,V,red(L1,K1,V1,R1)),
	K,V,black(L1,K1,V1,R1),done) :- !.
'$rb_delete_next'(black(black('',_,_,''),K,V,R),K,V,R,not_done) :- !.
'$rb_delete_next'(red(L,K,V,R),K0,V0,OUT,Done) :-
	'$rb_delete_next'(L,K0,V0,NL,Done0),
	'$rb_fixup_left'(Done0,red(NL,K,V,R),OUT,Done).
'$rb_delete_next'(black(L,K,V,R),K0,V0,OUT,Done) :-
	'$rb_delete_next'(L,K0,V0,NL,Done0),
	'$rb_fixup_left'(Done0,black(NL,K,V,R),OUT,Done).


'$rb_fixup_left'(done,T,T,done).
'$rb_fixup_left'(not_done,T,NT,Done) :-
	'$rb_fixup2'(T,NT,Done).


%
% case 1: x moves down, so we have to try to fix it again.
% case 1 -> 2,3,4 -> done
%
'$rb_fixup2'(black(black(Al,KA,VA,Be),KB,VB,red(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
	black(T1,KD,VD,black(Ep,KE,VE,Fi)),done) :- !,
	'$rb_fixup2'(red(black(Al,KA,VA,Be),KB,VB,black(Ga,KC,VC,De)),
		T1,
                _).
%
% case 2: x moves up, change one to red
%
'$rb_fixup2'(red(black(Al,KA,VA,Be),KB,VB,black(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
	black(black(Al,KA,VA,Be),KB,VB,red(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),done) :- !.
'$rb_fixup2'(black(black(Al,KA,VA,Be),KB,VB,black(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
	black(black(Al,KA,VA,Be),KB,VB,red(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),not_done) :- !.
%
% case 3: x stays put, shift left and do a 4
%
'$rb_fixup2'(red(black(Al,KA,VA,Be),KB,VB,black(red(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
	red(black(black(Al,KA,VA,Be),KB,VB,Ga),KC,VC,black(De,KD,VD,black(Ep,KE,VE,Fi))),
	done) :- !.
'$rb_fixup2'(black(black(Al,KA,VA,Be),KB,VB,black(red(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
	black(black(black(Al,KA,VA,Be),KB,VB,Ga),KC,VC,black(De,KD,VD,black(Ep,KE,VE,Fi))),
	done) :- !.
%
% case 4: rotate left, get rid of red
%
'$rb_fixup2'(red(black(Al,KA,VA,Be),KB,VB,black(C,KD,VD,red(Ep,KE,VE,Fi))),
	red(black(black(Al,KA,VA,Be),KB,VB,C),KD,VD,black(Ep,KE,VE,Fi)),
	done).
'$rb_fixup2'(black(black(Al,KA,VA,Be),KB,VB,black(C,KD,VD,red(Ep,KE,VE,Fi))),
	black(black(black(Al,KA,VA,Be),KB,VB,C),KD,VD,black(Ep,KE,VE,Fi)),
	done).


'$rb_fixup_right'(done,T,T,done).
'$rb_fixup_right'(not_done,T,NT,Done) :-
	'$rb_fixup3'(T,NT,Done).



%
% case 1: x moves down, so we have to try to fix it again.
% case 1 -> 2,3,4 -> done
%
'$rb_fixup3'(black(red(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	black(black(Fi,KE,VE,Ep),KD,VD,T1),done) :- !,
        '$rb_fixup3'(red(black(De,KC,VC,Ga),KB,VB,black(Be,KA,VA,Al)),T1,_).

%
% case 2: x moves up, change one to red
%
'$rb_fixup3'(red(black(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	black(red(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	done) :- !.
'$rb_fixup3'(black(black(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	black(red(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	not_done):- !.
%
% case 3: x stays put, shift left and do a 4
%
'$rb_fixup3'(red(black(black(Fi,KE,VE,Ep),KD,VD,red(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	red(black(black(Fi,KE,VE,Ep),KD,VD,De),KC,VC,black(Ga,KB,VB,black(Be,KA,VA,Al))),
	done) :- !.
'$rb_fixup3'(black(black(black(Fi,KE,VE,Ep),KD,VD,red(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	black(black(black(Fi,KE,VE,Ep),KD,VD,De),KC,VC,black(Ga,KB,VB,black(Be,KA,VA,Al))),
	done) :- !.
%
% case 4: rotate right, get rid of red
%
'$rb_fixup3'(red(black(red(Fi,KE,VE,Ep),KD,VD,C),KB,VB,black(Be,KA,VA,Al)),
	red(black(Fi,KE,VE,Ep),KD,VD,black(C,KB,VB,black(Be,KA,VA,Al))),
	done).
'$rb_fixup3'(black(black(red(Fi,KE,VE,Ep),KD,VD,C),KB,VB,black(Be,KA,VA,Al)),
	black(black(Fi,KE,VE,Ep),KD,VD,black(C,KB,VB,black(Be,KA,VA,Al))),
	done).


%
% whole list
%

%%	rb_visit(+T, -Pairs)
%
%	Pairs is an infix visit of tree   T, where each element of Pairs
%	is of the form K-Val.

rb_visit(t(_,T),Lf) :-
	'$rb_visit'(T,[],Lf).

rb_visit(t(_,T),L0,Lf) :-
	'$rb_visit'(T,L0,Lf).

'$rb_visit'(black('',_,_,_),L,L) :- !.
'$rb_visit'(red(L,K,V,R),L0,Lf) :-
	'$rb_visit'(L,[K-V|L1],Lf),
	'$rb_visit'(R,L0,L1).
'$rb_visit'(black(L,K,V,R),L0,Lf) :-
	'$rb_visit'(L,[K-V|L1],Lf),
	'$rb_visit'(R,L0,L1).

%%	rb_clone(+T, -NT, -Pairs)
%
%	"Clone" the red-back tree into a new  tree with the same keys as
%	the original but with all values set to unbound values. Nodes is
%	a list containing all new nodes as pairs K-V.

rb_clone(t(Nil,T),t(Nil,NT),Ns) :-
	'$rb_clone'(T,Nil,NT,Ns,[]).

'$rb_clone'(black('',_,_,''),Nil,Nil,Ns,Ns) :- !.
'$rb_clone'(red(L,K,_,R),Nil,red(NL,K,NV,NR),NsF,Ns0) :-
	'$rb_clone'(L,Nil,NL,NsF,[K-NV|Ns1]),
	'$rb_clone'(R,Nil,NR,Ns1,Ns0).
'$rb_clone'(black(L,K,_,R),Nil,black(NL,K,NV,NR),NsF,Ns0) :-
	'$rb_clone'(L,Nil,NL,NsF,[K-NV|Ns1]),
	'$rb_clone'(R,Nil,NR,Ns1,Ns0).

rb_clone(t(Nil,T),ONs,t(Nil,NT),Ns) :-
	'$rb_clone'(T,Nil,ONs,[],NT,Ns,[]).

'$rb_clone'(black('',_,_,''),Nil,ONs,ONs,Nil,Ns,Ns) :- !.
'$rb_clone'(red(L,K,V,R),Nil,ONsF,ONs0,red(NL,K,NV,NR),NsF,Ns0) :-
	'$rb_clone'(L,Nil,ONsF,[K-V|ONs1],NL,NsF,[K-NV|Ns1]),
	'$rb_clone'(R,Nil,ONs1,ONs0,NR,Ns1,Ns0).
'$rb_clone'(black(L,K,V,R),Nil,ONsF,ONs0,black(NL,K,NV,NR),NsF,Ns0) :-
	'$rb_clone'(L,Nil,ONsF,[K-V|ONs1],NL,NsF,[K-NV|Ns1]),
	'$rb_clone'(R,Nil,ONs1,ONs0,NR,Ns1,Ns0).


%
% whole keys
%
%%	rb_keys(+T, -Keys)
%
%	Keys is unified with  an  ordered  list   of  all  keys  in  the
%	Red-Black tree T.

rb_keys(t(_,T),Lf) :-
	'$rb_keys'(T,[],Lf).

rb_keys(t(_,T),L0,Lf) :-
	'$rb_keys'(T,L0,Lf).

'$rb_keys'(black('',_,_,''),L,L) :- !.
'$rb_keys'(red(L,K,_,R),L0,Lf) :-
	'$rb_keys'(L,[K|L1],Lf),
	'$rb_keys'(R,L0,L1).
'$rb_keys'(black(L,K,_,R),L0,Lf) :-
	'$rb_keys'(L,[K|L1],Lf),
	'$rb_keys'(R,L0,L1).


%%	list_to_rbtree(+L, -T) is det.
%
%	T is the red-black tree corresponding to the mapping in list L.

list_to_rbtree(List, T) :-
	sort(List,Sorted),
	ord_list_to_rbtree(Sorted, T).

%%	ord_list_to_rbtree(+L, -T) is det.
%
%	T is the red-black tree corresponding  to the mapping in ordered
%	list L.

ord_list_to_rbtree([], t(Nil,Nil)) :- !,
	Nil = black('', _, _, '').
ord_list_to_rbtree([K-V], t(Nil,black(Nil,K,V,Nil))) :- !,
	Nil = black('', _, _, '').
ord_list_to_rbtree(List, t(Nil,Tree)) :-
	Nil = black('', _, _, ''),
	Ar =.. [seq|List],
	functor(Ar,_,L),
	Height is truncate(log(L)/log(2)),
	construct_rbtree(1, L, Ar, Height, Nil, Tree).

construct_rbtree(L, M, _, _, Nil, Nil) :- M < L, !.
construct_rbtree(L, L, Ar, Depth, Nil, Node) :- !,
	arg(L, Ar, K-Val),
	'$rb_build_node'(Depth, Nil, K, Val, Nil, Node).
construct_rbtree(I0, Max, Ar, Depth, Nil, Node) :-
	I is (I0+Max)//2,
	arg(I, Ar, K-Val),
	'$rb_build_node'(Depth, Left, K, Val, Right, Node),
	I1 is I-1,
	NewDepth is Depth-1,
	construct_rbtree(I0, I1, Ar, NewDepth, Nil, Left),
	I2 is I+1,
	construct_rbtree(I2, Max, Ar, NewDepth, Nil, Right).

'$rb_build_node'( 0, Left, K, Val, Right, red(Left, K, Val, Right)) :- !.
'$rb_build_node'( _, Left, K, Val, Right, black(Left, K, Val, Right)).


%%	rb_size(+T, -Size) is det.
%
%	Size is the number of elements in T.

rb_size(t(_,T),Size) :-
	'$rb_size'(T,0,Size).

'$rb_size'(black('',_,_,_),Sz,Sz) :- !.
'$rb_size'(red(L,_,_,R),Sz0,Szf) :-
	Sz1 is Sz0+1,
	'$rb_size'(L,Sz1,Sz2),
	'$rb_size'(R,Sz2,Szf).
'$rb_size'(black(L,_,_,R),Sz0,Szf) :-
	Sz1 is Sz0+1,
	'$rb_size'(L,Sz1,Sz2),
	'$rb_size'(R,Sz2,Szf).

%%	is_rbtree(?Term) is semidet.
%
%	True if Term is a valide Red-Black tree.
%
%	@tbd	Catch variables.

is_rbtree(X) :-
	var(X), !, fail.
is_rbtree(t(Nil,Nil)) :- !.
is_rbtree(t(_,T)) :-
	catch(rbtree1(T), msg(_,_), fail).

%
% This code checks if a tree is ordered and a rbtree
%

rbtree1(black(L,K,_,R)) :-
	find_path_blacks(L, 0, Bls),
	check_rbtree(L,-inf,K,Bls),
	check_rbtree(R,K,+inf,Bls).
rbtree1(red(_,_,_,_)) :-
	throw(msg("root should be black",[])).


find_path_blacks(black('',_,_,''), Bls, Bls) :- !.
find_path_blacks(black(L,_,_,_), Bls0, Bls) :-
	Bls1 is Bls0+1,
	find_path_blacks(L, Bls1, Bls).
find_path_blacks(red(L,_,_,_), Bls0, Bls) :-
	find_path_blacks(L, Bls0, Bls).

check_rbtree(black('',_,_,''),Min,Max,Bls0) :- !,
	'$rb_check_height'(Bls0,Min,Max).
check_rbtree(red(L,K,_,R),Min,Max,Bls) :-
	'$rb_check_val'(K,Min,Max),
	check_red_child(L),
	check_red_child(R),
	check_rbtree(L,Min,K,Bls),
	check_rbtree(R,K,Max,Bls).
check_rbtree(black(L,K,_,R),Min,Max,Bls0) :-
	'$rb_check_val'(K,Min,Max),
	Bls is Bls0-1,
	check_rbtree(L,Min,K,Bls),
	check_rbtree(R,K,Max,Bls).

'$rb_check_height'(0,_,_) :- !.
'$rb_check_height'(Bls0,Min,Max) :-
	throw(msg("Unbalance ~d between ~w and ~w~n",[Bls0,Min,Max])).

'$rb_check_val'(K, Min, Max) :- ( K @> Min ; Min == -inf), (K @< Max ; Max == +inf), !.
'$rb_check_val'(K, Min, Max) :-
	throw(msg("not ordered: ~w not between ~w and ~w~n",[K,Min,Max])).

check_red_child(black(_,_,_,_)).
check_red_child(red(_,K,_,_)) :-
	throw(msg("must be red: ~w~n",[K])).
