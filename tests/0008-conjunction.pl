main :- a, b, c.

a :- true, put(97).

b :- fail, put(98).
b.

c :- put(99), fail, put(100).
c.
