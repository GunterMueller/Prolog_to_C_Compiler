%%% macros and other information about builtins


% predefined macros

% NOTE: macros may not introduce new variables - these are scanned
%       before a toplevel expression is compiled.

macro(command_line_arguments(X), foreign_call(command_line_arguments(X))).
macro(display(X), foreign_call(basic_write(current_output, X))).
macro(display(S, X), foreign_call(basic_write(S, X))).
macro(exists_file(NAME), foreign_call(file_exists(NAME))).
macro(exists_directory(NAME), foreign_call(dir_exists(NAME))).
macro(garbage_collect, foreign_call(gc)).
macro(halt(C), foreign_call(halt(C))).
macro(halt, foreign_call(halt(0))).
macro(nl, foreign_call(put_byte(current_output, 10))).
macro(nl(S), foreign_call(put_byte(S, 10))).
macro(put(BYTE), foreign_call(put_byte(current_output, BYTE))).
macro(put(S, BYTE), foreign_call(put_byte(S, BYTE))).
macro(put_byte(BYTE), foreign_call(put_byte(current_output, BYTE))).
macro(put_byte(S, BYTE), foreign_call(put_byte(S, BYTE))).
macro(put_code(BYTE), foreign_call(put_byte(current_output, BYTE))).
macro(put_code(S, BYTE), foreign_call(put_byte(S, BYTE))).
macro(get0(BYTE), foreign_call(get_byte(current_input, BYTE))).
macro(get0(S, BYTE), foreign_call(get_byte(S, BYTE))).
macro(get_byte(BYTE), foreign_call(get_byte(current_input, BYTE))).
macro(get_byte(S, BYTE), foreign_call(get_byte(S, BYTE))).
macro(get_code(BYTE), foreign_call(get_byte(current_input, BYTE))).
macro(get_code(S, BYTE), foreign_call(get_byte(S, BYTE))).
macro(peek_byte(BYTE), foreign_call(peek_byte(current_input, BYTE))).
macro(peek_byte(S, BYTE), foreign_call(peek_byte(S, BYTE))).
macro(peek_code(BYTE), foreign_call(peek_byte(current_input, BYTE))).
macro(peek_code(S, BYTE), foreign_call(peek_byte(S, BYTE))).
macro(erase(REF), foreign_call(db_erase(REF))).
macro(getenv(NAME, VAL), foreign_call(get_environment_variable(NAME, VAL))).
macro(shell(CMD, STATUS), foreign_call(shell_command(CMD, STATUS))).
macro(functor(T, N, A), foreign_call(functor(T, N, A))).
macro(arg(I, T, X), foreign_call(term_arg(I, T, X))).
macro(seeing(S), foreign_call(current_input_stream(S))).
macro(telling(S), foreign_call(current_output_stream(S))).
macro(current_input(S), foreign_call(current_input_stream(S))).
macro(current_output(S), foreign_call(current_output_stream(S))).
macro(current_error_output(S), foreign_call(current_error_stream(S))).
macro(read(T), '$read1'(T)).
macro(read(S, T), '$read2'(S, T)).
macro(enable_trace(F), foreign_call(enable_trace(F))).
macro(getpid(PID), foreign_call(get_process_id(PID))).
macro(sleep(SECS), foreign_call(sleep_for_seconds(SECS))).
macro(set_random_state(SEED), foreign_call(set_random_seed(SEED))).
macro(atom_hash(ATOM, HASH), foreign_call(atom_hash(ATOM, HASH))).
macro(acyclic_term(X), foreign_call(acyclic_term(X))).
macro(close(S), foreign_call(close_stream(S))).
macro(close(S, _), close(S)).
macro(atom_length(A, L), foreign_call(atom_length(A, L))).
macro(ground(X), foreign_call(ground(X))).
macro(rename_file(X, Y), foreign_call(rename_file(X, Y))).
macro(delete_file(X), foreign_call(delete_file(X))).
macro(stream_property(S, P), '$stream_property'(P, S)). % to utilize indexing
macro(getcwd(X), foreign_call(get_working_dir(X))).
macro(chdir(X), foreign_call(set_working_dir(X))).
macro(char_code(A, C), atom_codes(A, [C])).


% nothing matches - tryi auto-include and finally, fail
macro(TERM, TERM) :-
	functor(TERM, NAME, ARITY),
	atom(NAME),
	auto_include(NAME, ARITY, FILE),
	add_boilerplate(FILE, (:- include(library(FILE)))).


%% auto-include definitions

auto_include(length, 2, 'lists').
auto_include(append, 2, 'lists').
auto_include(append, 3, 'lists').
auto_include(member, 2, 'lists').
auto_include(reverse, 2, 'lists').
auto_include(memberchk, 2, 'lists').
auto_include(is_list, 1, 'lists').
auto_include(slice, 4, 'lists').
auto_include(nth, 3, 'lists').

auto_include(compare, 3, 'misc').
auto_include(shell, 1, 'misc').
auto_include(between, 3, 'misc').
auto_include(atom_codes, 2, 'misc').
auto_include(number_codes, 2, 'misc').
auto_include(atom_number, 2, 'misc').

auto_include(atomic_list_concat, 2, 'str').
auto_include(split_string, 4, 'str').
auto_include(sub_atom, 5, 'str').

auto_include(tab, 1, 'io').
auto_include(skip, 1, 'io').
auto_include(get, 1, 'io').
auto_include(see, 1, 'io').
auto_include(seen, 0, 'io').
auto_include(seeing, 1, 'io').
auto_include(append, 1, 'io').
auto_include(tell, 1, 'io').
auto_include(told, 0, 'io').
auto_include(telling, 1, 'io').
auto_include(open, 3, 'io').
auto_include(open, 4, 'io').
auto_include(read_string, 2, 'io').
auto_include(read_string, 3, 'io').
auto_include(read_line, 1, 'io').
auto_include(read_line, 2, 'io').
auto_include(flush_output, 0, 'io').
auto_include(flush_output, 1, 'io').
auto_include(at_end_of_stream, 0, 'io').
auto_include(at_end_of_stream, 1, 'io').
auto_include(set_input, 0, 'io').
auto_include(set_output, 1, 'io').
auto_include(set_stream_position, 2, 'io').
auto_include('$stream_property', 2, 'io').
	     
auto_include(op, 3, 'op').
auto_include(current_op, 3, 'op').

auto_include(throw, 1, 'misc').
auto_include(name, 2, 'misc').
auto_include('=..', 2, 'misc').
auto_include(deref_term, 4, 'misc').
auto_include(copy_term, 2, 'misc').
auto_include(duplicate_term, 2, 'misc').
auto_include(unify_with_occurs_check, 2, 'misc').

auto_include(union, 3, 'sets').
auto_include(intersection, 3, 'sets').
auto_include(subtract, 3, 'sets').
auto_include(select, 3, 'sets').
auto_include(symdiff, 3, 'sets').

auto_include(write, 1, 'write').
auto_include(writeq, 1, 'write').
auto_include(write, 2, 'write').
auto_include(writeq, 2, 'write').

auto_include(recorda, 2, 'rdb').
auto_include(recorda, 3, 'rdb').
auto_include(recordz, 2, 'rdb').
auto_include(recordz, 3, 'rdb').
auto_include(recorded, 2, 'rdb').
auto_include(recorded, 3, 'rdb').

auto_include('$findall_start', 0, 'findall').
auto_include('$findall_push', 1, 'findall').
auto_include('$findall_collect', 1, 'findall').
auto_include('$bagof_start', 3, 'findall').
auto_include('$bagof_finish', 1, 'findall').

auto_include(sort, 2, 'sorts').
auto_include(keysort, 2, 'sorts').
auto_include(merge, 3, 'sorts').

auto_include(list_to_ord_set, 2, 'ordset').
auto_include(ord_disjoint, 2, 'ordset').
auto_include(ord_insert, 3, 'ordset').
auto_include(ord_intersect, 2, 'ordset').
auto_include(ord_intersect, 3, 'ordset').
auto_include(ord_subset, 2, 'ordset').
auto_include(ord_symdiff, 2, 'ordset').
auto_include(ord_union, 2, 'ordset').
auto_include(ord_subtract, 2, 'ordset').
auto_include(ord_memberchk, 2, 'ordset').

auto_include(read_tokens, 2, 'rdtok').
auto_include('$read1', 1, 'read').
auto_include('$read1', 2, 'read').
auto_include('$read2', 2, 'read').

auto_include(clause, 2, 'cdb').
auto_include(clause, 3, 'cdb').
auto_include(retract, 1, 'cdb').
auto_include(abolish, 1, 'cdb').
auto_include(asserta, 1, 'cdb').
auto_include(asserta, 2, 'cdb').
auto_include(assertz, 1, 'cdb').
auto_include(assertz, 2, 'cdb').

auto_include(succ, 2, 'arith').
auto_include(plus, 3, 'arith').
auto_include(times, 3, 'arith').
auto_include(divide, 4, 'arith').

auto_include(writef, 1, 'writef').
auto_include(writef, 2, 'writef').
auto_include(fwritef, 2, 'writef').
auto_include(fwritef, 3, 'writef').

auto_include('$delay_goal', 4, 'co').
auto_include('$freeze_goal', 4, 'co').
auto_include(dif, 2, 'co').
auto_include(all_different, 1, 'co').

auto_include(current_prolog_flag, 2, 'flags').

auto_include(atom_concat, 3, 'iso').
auto_include(atom_chars, 2, 'iso').
auto_include(number_chars, 2, 'iso').
auto_include(get_char, 1, 'iso').
auto_include(get_char, 2, 'iso').
auto_include(peek_char, 1, 'iso').
auto_include(peek_char, 2, 'iso').
auto_include(put_char, 1, 'iso').
auto_include(put_char, 2, 'iso').

auto_include(numbervars, 3, 'numvars').

auto_include(read_term, 2, 'readt').
auto_include(read_term, 3, 'readt').

auto_include(_, _, _) :- fail.


%% add boilerplate code for a given tag, unless already added

add_boilerplate(TAG, _) :-
	recorded(boilerplate_added, TAG), !.
add_boilerplate(TAG, CODE) :-
	recordz(boilerplate, CODE),
	recordz(boilerplate_added, TAG).


%% determinateness information

determinate_builtin(memberchk, 2).
determinate_builtin(compare, 3).
determinate_builtin(shell, 1).
determinate_builtin(tab, 1).
determinate_builtin(tab, 2).
determinate_builtin(skip, 1).
determinate_builtin(skip, 2).
determinate_builtin(get, 1).
determinate_builtin(get, 2).
determinate_builtin(see, 1).
determinate_builtin(seen, 0).
determinate_builtin(seeing, 1).
determinate_builtin(tell, 1).
determinate_builtin(append, 1).
determinate_builtin(told, 0).
determinate_builtin(telling, 1).
determinate_builtin(op, 3).
determinate_builtin(throw, 1).
determinate_builtin(name, 2).
determinate_builtin('=..', 2).
determinate_builtin(deref_term, 3).
determinate_builtin(copy_term, 2).
determinate_builtin(duplicate_term, 2).
determinate_builtin(write, 1).
determinate_builtin(writeq, 1).
determinate_builtin(write, 2).
determinate_builtin(writeq, 2).
determinate_builtin(recorda, 2).
determinate_builtin(recorda, 3).
determinate_builtin(recordz, 2).
determinate_builtin(recordz, 3).
determinate_builtin('$findall_start', 0).
determinate_builtin('$findall_push', 1).
determinate_builtin('$findall_collect', 1).
determinate_builtin(ord_memberchk, 2).
determinate_builtin(ord_disjoint, 2).
determinate_builtin(ord_insert, 3).
determinate_builtin(ord_intersect, 3).
determinate_builtin(ord_subset, 2).
determinate_builtin(ord_subtract, 3).
determinate_builtin(ord_symdiff, 3).
determinate_builtin(ord_union, 3).
determinate_builtin(read_tokens, 2).
determinate_builtin('$read1', 1).
determinate_builtin('$read1', 2).
determinate_builtin('$read2', 1).
determinate_builtin(abolish, 1).
determinate_builtin(asserta, 1).
determinate_builtin(asserta, 2).
determinate_builtin(assertz, 1).
determinate_builtin(assertz, 2).
determinate_builtin(open, 3).
determinate_builtin(open, 4).
determinate_builtin(succ, 2).
determinate_builtin(plus, 2).
determinate_builtin(times, 3).
determinate_builtin(divide, 4).
determinate_builtin(writef, 1).
determinate_builtin(writef, 2).
determinate_builtin(fwritef, 2).
determinate_builtin(fwritef, 3).
determinate_builtin(read_string, 2).
determinate_builtin(read_line, 1).
determinate_builtin(read_string, 3).
determinate_builtin(read_line, 2).
determinate_builtin(dif, 2).
determinate_builtin(unify_with_occurs_check, 2).
determinate_builtin(flush_output, 0).
determinate_builtin(flush_output, 1).
determinate_builtin(set_input, 1).
determinate_builtin(set_output, 1).
determinate_builtin(at_end_of_stream, 0).
determinate_builtin(at_end_of_stream, 1).
determinate_builtin(atom_chars, 2).
determinate_builtin(get_char, 1).
determinate_builtin(get_char, 2).
determinate_builtin(peek_char, 1).
determinate_builtin(peek_char, 2).
determinate_builtin(put_char, 1).
determinate_builtin(put_char, 2).
determinate_builtin(atom_codes, 2).
determinate_builtin(number_codes, 2).
determinate_builtin(atom_number, 2).
determinate_builtin(is_list, 1).
determinate_builtin(atomic_list_concat, 2).
determinate_builtin(split_string, 4).
determinate_builtin(numbervars, 3).
determinate_builtin(set_stream_position, 2).
determinate_builtin(atom_chars, 2).
determinate_builtin(number_chars, 2).
determinate_builtin(plus, 3).
determinate_builtin(succ, 2).
determinate_builtin(times, 3).
determinate_builtin(divide, 4).
determinate_builtin(union, 3).
determinate_builtin(intersection, 3).
determinate_builtin(subtract, 3).
determinate_builtin(symdiff, 3).
		    
determinate_builtin(NAME, ARITY) :-
	recorded(determinate_predicate, NAME/ARITY).


%% check if N/A refers to registered meta-predicate

is_meta_predicate(NAME, ARITY, SIG) :-
	recorded(meta_signature, info(NAME, ARITY, SIG)).


%% is expression a simple determinate test?

simple_test(P) :-
	functor(P, N, A),
	( A == 1, type_predicate(N)
	; A == 2, comparison_predicate(N)
	).
simple_test((X, Y)) :-
	simple_test(X),
	simple_test(Y).


%% classifications for some builtin predicates

type_predicate(number).
type_predicate(atomic).
type_predicate(atom).
type_predicate(number).
type_predicate(integer).
type_predicate(compound).
type_predicate(float).
type_predicate(var).
type_predicate(nonvar).
type_predicate(is_stream).
type_predicate(db_reference).
type_predicate(foreign_pointer).

comparison_predicate('@>').
comparison_predicate('@<').
comparison_predicate('@>=').
comparison_predicate('@=<').
comparison_predicate('=:=').
comparison_predicate('=\\=').
comparison_predicate('>').
comparison_predicate('<').
comparison_predicate('>=').
comparison_predicate('=<').
comparison_predicate('=').
comparison_predicate('\\=').
comparison_predicate('==').
comparison_predicate('\\==').
