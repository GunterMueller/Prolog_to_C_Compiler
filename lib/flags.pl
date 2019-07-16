%%% prolog flags


current_prolog_flag(version, 7).
current_prolog_flag(prolog_title, '?-Prolog').
current_prolog_flag(prolog_copyright, '(c)MMXV Felix L. Winkelmann').
current_prolog_flag(bounded, true).
current_prolog_flag(max_arity, 65536). % arbitrary
current_prolog_flag(integer_rounding_function, toward_zero). %XXX correct?
current_prolog_flag(max_integer, N) :- foreign_call(fixnum_bounds(_, N)).
current_prolog_flag(min_integer, N) :- foreign_call(fixnum_bounds(N, _)).
current_prolog_flag(unknown, error).
current_prolog_flag(unix, F) :- (foreign_call(os_type(0)) -> F = true; F = false).
current_prolog_flag(windows, F) :- (foreign_call(os_type(1)) -> F = true; F = false).
current_prolog_flag(apple, F) :- (foreign_call(os_type(2)) -> F = true; F = false).
current_prolog_flag(address_bits, N) :- foreign_call(word_size(N)).
