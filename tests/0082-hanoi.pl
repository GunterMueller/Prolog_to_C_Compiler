%	towers of hanoi ( hanoi ) for 8 disks

hanoi :- hanoi(8).

hanoi(N) :- move(N,left,center,right).

move(0,_,_,_) :- !.
move(N,A,B,C) :- M is N-1, move(M,A,C,B), inform(A,B), move(M,C,B,A).

inform(A,B) :- write([move,disk,from,A,to,B]), nl, fail.
inform(_,_).

:- initialization hanoi.


