main :-
	catch(display(1), X, display(2)), nl,
	catch(throw(123), X, display(X)), nl,
	catch(member(Y,[1,2,3]), _, true), display(Y), nl, fail.
main :-
	catch(p, X, display('bad\n')),
	catch(fail, _, nl).

p :-
	display('p\n'),
	catch(q, yo, display('good\n')),
	nl.
	

q :-
	throw(yo).
