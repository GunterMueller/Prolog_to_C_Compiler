%	differen (times10,divide10,log10,ops8)
%	These 4 examples are from Warren's thesis

diff :-
	times10(I1),
	d(I1,x,D1),
	write(D1), nl,
	divide10(I2),
	d(I2,x,D2),
	write(D2), nl,
	log10(I3),
	d(I3,x,D3),
	write(D3), nl,
	ops8(I4),
	d(I4,x,D4),
	write(D4), nl.

d(U+V,X,DU+DV) :- !, d(U,X,DU), d(V,X,DV).
d(U-V,X,DU-DV) :- !, d(U,X,DU), d(V,X,DV).
d(U*V,X,DU*V+U*DV) :- !, d(U,X,DU), d(V,X,DV).
d(U/V,X,(DU*V-U*DV)/(^(V,2))) :- !, d(U,X,DU), d(V,X,DV).
d(^(U,N),X,DU*N*(^(U,N1))) :- !, integer(N), N1 is N - 1, d(U,X,DU).
d(-U,X,-DU) :- !, d(U,X,DU).
d(exp(U),X,exp(U)*DU) :- !, d(U,X,DU).
d(log(U),X,DU/U) :- !, d(U,X,DU).
d(X,X,1).	% There is a cut in Warren's program! -- Jacques
d(C,X,0).

times10( ((((((((x*x)*x)*x)*x)*x)*x)*x)*x)*x ).
divide10( ((((((((x/x)/x)/x)/x)/x)/x)/x)/x)/x ).
log10( log(log(log(log(log(log(log(log(log(log(x)))))))))) ).
ops8( (x+1)*((^(x,2)+2)*(^(x,3)+3)) ).

:- initialization diff.
