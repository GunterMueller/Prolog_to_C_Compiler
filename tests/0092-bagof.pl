p(1,3,5).
p(2,4,1).
p(3,5,2).
p(4,3,1).
p(5,2,4).

main :-
	bagof(Z,p(X,Y,Z),Bag),
	show(X, Y, Z, Bag),
	fail.
main :-
	findall(Z,p(X,Y,Z),Bag),
	show(X, Y, Z, Bag),
	fail.
main :-
	bagof(Z,X^Y^p(X,Y,Z),Bag),
	show(X, Y, Z, Bag),
	fail.
main :-
	setof(Z,X^Y^p(X,Y,Z),Bag),
	show(X, Y, Z, Bag),
	fail.

show(X, Y, Z, Bag) :-    
    	display('X = '), write(X), display(', Y = '), write(Y), display(', Z = '), write(Z),
	display(', Bag = '), write(Bag), nl.
