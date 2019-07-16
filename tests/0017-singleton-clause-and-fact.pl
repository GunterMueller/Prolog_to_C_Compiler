% this exposed a bug in process-input, where the new block for foo/0
% incorrectly contained the previously compiled block.

main :- foo.
foo.
