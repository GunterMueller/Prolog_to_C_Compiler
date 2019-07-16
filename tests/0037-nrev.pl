% nrev/2 - naive reverse.
nrev([ ],[ ]).
nrev([X|Xs],Zs) :-
         nrev(Xs,Ys),
         append(Ys,[X],Zs).

% nrev 30 has 496 logical inferences
nrev30 :-
	nrev([1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0], X),
	display(X), nl.

:- initialization(nrev30).
