main :-
	exists_file('/etc/fstab'),
	\+exists_file("/tmp").
