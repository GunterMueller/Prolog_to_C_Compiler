% Example using two accumulators:

acc_info(code, T, Out, In, (Out=[T|In])).
acc_info(size, T, In, Out, (Out is T+In)).

pred_info(expr_code, 1, [size,code]).

expr_code(A+B) -->>
        expr_code(A),
        expr_code(B),
        [plus]:code,    % Accumulate 'plus' in the code accumulator.
        [1]:size.       % Accumulate 1 in the size accumulator.
expr_code(I) -->>
        {atomic(I)},
        [push(I)]:code,
        [1]:size.

main :-
	expr_code(a+(1+b+c)+33, 0, S, C, []),
	write(S), nl,
	write(C), nl.
