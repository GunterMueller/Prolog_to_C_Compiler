main :- (true -> put(65); put(66)),
	(fail -> put(67); put(68)),
	foo.

foo :- (bar -> put(48); true), fail.
foo :- put(49).

bar :- put(97).
bar :- put(98).
