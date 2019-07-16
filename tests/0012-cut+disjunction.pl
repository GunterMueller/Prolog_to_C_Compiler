main :- foo.
main :- put(67).

foo :- (!, fail; put(65)).
foo :- put(66).
