%%% list processing operations


:- mode slice(?, +, +, ?),
	nth(?, +, ?).


member(X, [X|_]).
member(X, [_|R]) :- member(X, R).

append([],X,X).
append([X|Y],Z,[X|W]) :- append(Y,Z,W).

append([], []).
append([X|Y], Z) :-
	append(Y, Z2),
	append(X, Z2, Z).

reverse(L, R) :- reverse(L, R, []).
reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

length([], 0).
length([_|X], N) :- length(X, N2), N is N2 + 1.

%% needs to be wrapped into predicate, otherwise vars bound are not trailed properly
memberchk(X, Y) :- foreign_call(memberchk(X, Y)).

is_list(List) :- nonvar(List), (List == []; List = [_|_]).

%% from: https://sites.google.com/site/prologsite/home

% 1.18 (**):  Extract a slice from a list

% slice(L1,I,K,L2) :- L2 is the list of the elements of L1 between
%    index I and index K (both included).
%    (list,integer,integer,list) (?,+,+,?)

slice([X|_],1,1,[X]).
slice([X|Xs],1,K,[X|Ys]) :-
	K > 1, 
	K1 is K - 1,
	slice(Xs,1,K1,Ys).
slice([_|Xs],I,K,Ys) :-
	I > 1, 
	I1 is I - 1,
	K1 is K - 1,
	slice(Xs,I1,K1,Ys).

%   nth(Elem, List, Index) Possible Calling Sequences
%   nth(+,+,-) or nth(-,+,+) or nth(-,+,-).
%   True when Elem is the Indexth member of List.
%   It may be used to select a particular element, or to find where some
%   given element occurs, or to enumerate the elements and indices togther.

nth(1, [Elem|_], Elem).
nth(N, [_|List], Elem) :-
	nth(M, List, Elem),
	N is M+1.
