main :- foo, bar, baz.

foo :- fail; display('1').
bar :- (twice, display('2')
       ; fail).
bar.

twice :- display('3').
twice :- display('4').

baz :- true; display('5').
