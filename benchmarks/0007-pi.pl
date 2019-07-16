:- include('lib/interp.pl').

main :-
	pi_init(['.']),
	consult('tests/0085-boyer.pl'),
	call(main).
