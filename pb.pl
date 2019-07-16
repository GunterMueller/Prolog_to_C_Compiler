%%% bindings generator
%
%
% Currently understands the following:
%
% "#" ... ["\" \n ...]
% [STORAGE] TYPE IDENTIFIER [("[" [INTEGER] "]") ...] "," ... [";"]
% [STORAGE] [MODE] TYPE IDENTIFIER "(" ([DIRECTION] TYPE [IDENTIFIER [("[" [INTEGER] "]") ...]]) "," ... ")" [";"]
%
% TYPE = ["const"] BASETYPE ("const" | "*") ...
% STORAGE = "extern" | "static"
% DIRECTION = "/*" ("in" | "out") "*/"
% MODE = "/*" ("success" | "fail" | "fail:" IDENTIFIER ) "*/"
% BASETYPE = ["unsigned"] ("int" | "char" | "short" | "long" | "float" | "double" | "void") 


%% parsing of definitions

parse_definition(none, _, _, _) -->
	ws, "#", verbatim_block(BLOCK),
	{add_verbatim_block([35|BLOCK])}.
parse_definition(NAME, REALNAME, RTYPE, ARGTYPES) -->
	definition_prefix(RTYPE1), ws, entity_name(NAME, REALNAME),
	ws, parse_suffix(RTYPE1, RTYPE, ARGTYPES),
	{store_definition(NAME, REALNAME, RTYPE, ARGTYPES)},
	ws, next_definition(RTYPE).

definition_prefix(RTYPE) --> ws, storage, ws1, result_type(RTYPE).
definition_prefix(RTYPE) --> ws1, result_type(RTYPE).
definition_prefix(RTYPE) --> ws, result_type(RTYPE).

next_definition(RTYPE1) -->
	",", ws, entity_name(NAME, REALNAME), ws,
	parse_suffix(RTYPE1, RTYPE, ARGTYPES),
	{store_definition(NAME, REALNAME, RTYPE, ARGTYPES)},
	!, ws, next_definition(RTYPE).
next_definition(_) --> (";"; "").

verbatim_block([92,10|R]) --> "\\", ws0, [10], skip_line, verbatim_block(R).
verbatim_block([10]) --> [10].
verbatim_block([C|R]) --> [C], verbatim_block(R).

parse_suffix(RTYPE, FINALRTYPE, ARGTYPES) --> indices(RTYPE, FINALRTYPE).
parse_suffix(RTYPE, RTYPE, ARGTYPES) --> arguments(ARGTYPES).
parse_suffix(RTYPE, RTYPE, none) --> [].

storage --> ("extern"; "static").

%% whitespace without comments
ws0([C|R], OUT) :- memberchk(C, [32, 13, 9]), !, ws0(R, OUT).
ws0 --> "".

%% whitespace with line-comments
ws1([C|R], OUT) :- memberchk(C, [32, 10, 13, 9]), !, ws1(R, OUT).
ws1 --> "//", skip_line, !, ws1.
ws1 --> [].

%% any type of whitespace
ws --> ws1, (ws2; "").

%% block comment
ws2 --> "/*", skip_comment, !, ws.

skip_line --> [10], !.
skip_line --> [_], !, skip_line.

skip_comment --> "*/", !.
skip_comment --> [_], !, skip_comment.

eof(IN) :- ws(IN, []).

result_type(success(RTYPE)) --> "/*", ws1, "success", ws1, "*/", ws, type(RTYPE).
result_type(fail(RTYPE, SVAR)) --> "/*", ws1, "fail:", ws1, identifier(SVAR), ws1, "*/", ws, type(RTYPE).
result_type(fail(RTYPE)) --> "/*", ws1, "fail", ws1, "*/", ws, type(RTYPE).
result_type(TYPE) --> type(TYPE).

identifier(ID, [C|IN], OUT) :-
	(uppercase(C); lowercase(C); C == 95),
	identifier_chars(IN, OUT, IR),
	!, name(ID, [C|IR]).

identifier_chars([C|IN], OUT, [C|MORE]) :-
	(uppercase(C); lowercase(C); digit(C); C == 95),
	identifier_chars(IN, OUT, MORE).
identifier_chars(IN, IN, "").

numeric_literal(NUM, [C|IN], OUT) :-
	digit(C),
	number_chars(IN, OUT, IR),
	number_codes(NUM, [C|IR]).

number_chars([C|IN], OUT, [C|MORE]) :- digit(C), number_chars(IN, OUT, MORE).
number_chars(IN, IN, "").

entity_name(NAME, RNAME) --> identifier(NAME), ws1, real_identifier(NAME, RNAME).

real_identifier(_, RNAME) --> "/*", ws1, "=>", ws, identifier(RNAME), ws1, "*/".
real_identifier(NAME, NAME) --> [].

indices(RTYPE, FINALRTYPE) -->
	"[", ws, (numeric_literal(_); ""), ws, "]",
	(indices(pointer(RTYPE), FINALRTYPE); {FINALRTYPE = pointer(RTYPE)}).

arguments([]) --> "(", ws, ")".
arguments(ARGTYPES) --> "(", ws1, argument_type_list(ARGTYPES), ws, ")".

direction(in) --> "/*", ws1, "in", ws1, "*/".
direction(out) --> "/*", ws1, "out", ws1, "*/".
direction(in) --> [].

argument_type_list([TYPE|MORE]) -->
	argument_type(TYPE), ws, ",", ws1, argument_type_list(MORE).
argument_type_list([TYPE]) -->
	argument_type(TYPE).

argument_type(TYPE) -->
	direction(DIR), ws, type(TYPE1), ws,
	(identifier(_), argument_type_suffix(TYPE1, TYPE2); {TYPE2 = TYPE1}),
	{apply_direction(DIR, TYPE2, TYPE)}.

apply_direction(in, T, T).
apply_direction(out, T, out(T)).

argument_type_suffix(T1, T2) --> indices(T1, T2).
argument_type_suffix(T, T) --> [].

type(TYPE) -->
	type_prefixes(PREFIXED, BASETYPE), ws, identifier(BASETYPE), ws, type_qualifiers(PREFIXED, TYPE).

type_prefixes(const(TYPE), VAR) -->
	identifier(const), ws, type_prefixes(TYPE, VAR).
type_prefixes(unsigned(TYPE), VAR) -->
	identifier(unsigned), ws, type_prefixes(TYPE, VAR).
type_prefixes(VAR, VAR) --> [].

type_qualifiers(BASETYPE, TYPE) -->
	"*", ws, type_qualifiers(pointer(BASETYPE), TYPE).
type_qualifiers(BASETYPE, TYPE) -->
	identifier(const), ws, type_qualifiers(const(BASETYPE), TYPE).
type_qualifiers(TYPE, TYPE) --> [].


%% maintain stored definition info

store_definition(NAME, REALNAME, RESULTTYPE, none) :-
	(recorded(silent, _)
	; display('% '), write(variable(REALNAME, RESULTTYPE)), nl
	),
	recordz(definitions, variable(NAME, REALNAME, RESULTTYPE)).
store_definition(NAME, REALNAME, RESULTTYPE, ARGTYPES) :-
	(recorded(silent, _)
	; display('% '), write(function(REALNAME, RESULTTYPE, ARGTYPES)), nl
	),
	recordz(definitions, function(NAME, REALNAME, RESULTTYPE, ARGTYPES)).

add_verbatim_block(LST) :-
	name(CODE, LST), recordz(verbatim_block, CODE).


%% generate file with primitives

generate_header_file(FNAME) :-
	tell(FNAME),
	generate_verbatim_code,
	generate_primitives,
	told.

generate_verbatim_code :-
	recorded(verbatim_block, CODE),
	gen(CODE),
	fail.
generate_verbatim_code.

generate_primitives :-
	recorded(definitions, variable(NAME, REALNAME, RTYPE)),
	variable_primitive(NAME, REALNAME, RTYPE),
	fail.
generate_primitives :-
	recorded(definitions, function(NAME, REALNAME, RTYPE, ARGTYPES)),
	function_primitive(NAME, REALNAME, RTYPE, ARGTYPES),
	fail.
generate_primitives.

variable_primitive(NAME, REALNAME, const(T)) :-
	variable_getter(NAME, REALNAME, T), !.
variable_primitive(NAME, REALNAME, RTYPE) :-
	variable_getter(NAME, REALNAME, RTYPE),
	variable_setter(NAME, REALNAME, RTYPE), !.
	
variable_getter(NAME, REALNAME, RTYPE) :-	
	gen('\nPRIMITIVE(v_', REALNAME, ',X x){\nreturn unify('),
	p_value(RTYPE, NAME),
	gen(',x);}\n').

variable_setter(NAME, REALNAME, RTYPE) :-
	gen('\nPRIMITIVE(set_v_', REALNAME, ',X x){\n', NAME, '='),
	c_value(RTYPE, 'x'),
	gen(';\nreturn 1;}\n').

function_primitive(NAME, REALNAME, void, []) :-
	gen('\nstatic inline int f_', REALNAME, '(CHOICE_POINT *C0){\n'),
	gen(NAME, '();\nreturn 1;}\n'), !.
function_primitive(NAME, REALNAME, RTYPE, ARGTYPES) :-
	gen('\static inline int f_', REALNAME, '(CHOICE_POINT *C0'),
	length(ARGTYPES, ARGC),
	forall(between(1, ARGC, I), gen(',X x', I)),
	gen_result_arg(RTYPE),
	gen('){\n'),
	gen_out_vars(1, ARGTYPES),
	gen_call(RTYPE, NAME, ARGTYPES, 'x'),
	gen_exit_or_fail(RTYPE),
	gen_out_results(1, ARGTYPES),
	gen_return(RTYPE), gen('}\n'), !.

gen_exit_or_fail(success(_)) :- gen('if(!x) return 0;\n').
gen_exit_or_fail(fail(_, SVAR)) :- gen('if((', SVAR, '=x)!=0) return 0;\n').
gen_exit_or_fail(fail(_)) :- gen('if(x) return 0;\n').
gen_exit_or_fail(_).

gen_return(void) :- gen('return 1;\n').
gen_return(success(_)) :- gen('return 1;\n').
gen_return(fail(_)) :- gen('return 1;\n').
gen_return(fail(_, _)) :- gen('return 1;\n').
gen_return(RTYPE) :- gen('return unify('), p_value(RTYPE, 'x'), gen(',r);\n').

gen_result_arg(success(_)).
gen_result_arg(fail(_)).
gen_result_arg(fail(_, _)).
gen_result_arg(void).
gen_result_arg(_) :- gen(',X r').
	       
gen_type(pointer(T)) :- !, gen_type(T), gen('*').
gen_type(const(T)) :- !, gen('const '), gen_type(T).
gen_type(unsigned(T)) :- !, gen('unsigned '), gen_type(T).
gen_type(success(T)) :- !, gen_type(T).
gen_type(fail(T)) :- !, gen_type(T).
gen_type(fail(T, _)) :- !, gen_type(T).
gen_type(T) :- gen(T).

gen_call(RTYPE, NAME, ARGTYPES, RESULT) :-
	(RTYPE == void; gen_type(RTYPE), gen(' ', RESULT, '=')),
	gen(NAME, '('),
	gen_call(1, ARGTYPES),
	gen(');\n').

gen_call(_, []).
gen_call(I, [ATYPE]) :-
	gen_call_arg(I, ATYPE).
gen_call(I, [ATYPE|R]) :-
	gen_call_arg(I, ATYPE),
	gen(','),
	I2 is I + 1,
	gen_call(I2, R).

gen_call_arg(I, out(T)) :-
	number_codes(I, IL),
	name(ARG, [114|IL]),	% "r"
	gen('&', ARG).
gen_call_arg(I, TYPE) :-
	number_codes(I, IL),
	name(ARG, [120|IL]),	% "x"
	c_value(TYPE, ARG).

gen_out_vars(_, []).
gen_out_vars(I, [out(pointer(T))|R]) :-
	number_codes(I, IL),
	name(VAR,[114|IL]), 	% "r"
	gen_type(T), gen(' ', VAR, ';\n'),
	I2 is I + 1,
	gen_out_vars(I2, R).
gen_out_vars(I, [_|R]) :-
	I2 is I + 1,
	gen_out_vars(I2, R).
	
gen_out_results(_, []).
gen_out_results(I, [out(pointer(T))|R]) :-
	number_codes(I, IL),
	name(VAR,[114|IL]), 	% "r"
	gen('if(!unify('), p_value(T, VAR), gen(',x', I, ')) return 0;\n'),
	I2 is I + 1,
	gen_out_results(I2, R).
gen_out_results(I, [_|R]) :-
	I2 is I + 1,
	gen_out_results(I2, R).
	

%% generate file with wrapper predicates

generate_wrapper_file(FNAME, HNAME) :-
	tell(FNAME),
	name(HFILE, HNAME),
	( \+recorded(include_source, yes)
	; recorded(source_file, FN),
	  gen(':- verbatim(\'#include \"', FN, '\"\').\n')
	),
	gen(':- verbatim(\'#include "', HFILE, '"\').\n'),
	generate_wrappers,
	told.

generate_wrappers :-
	recorded(definitions, variable(_, REALNAME, RTYPE)),
	variable_wrapper(REALNAME, RTYPE),
	fail.
generate_wrappers :-
	recorded(definitions, function(_, REALNAME, RTYPE, ARGTYPES)),
	function_wrapper(REALNAME, RTYPE, ARGTYPES),
	fail.
generate_wrappers.

variable_wrapper(NAME, const(_)) :-
	variable_getter_wrapper(NAME), !.
variable_wrapper(NAME, _) :-
	variable_getter_wrapper(NAME),
	variable_setter_wrapper(NAME), !.

variable_getter_wrapper(NAME) :-
	gen(':- determinate(', NAME, '/1).\n'),
	gen(NAME, '(X) :- foreign_call(v_', NAME, '(X)).\n').
variable_setter_wrapper(NAME) :-	
	gen(':- determinate(set_', NAME, '/1).\n'),
	gen('set_', NAME, '(X) :- foreign_call(set_v_', NAME, '(X)).\n').

function_wrapper(NAME, void, []) :-
	gen(':- determinate(', NAME, '/0).\n'),
	gen(NAME, ' :- foreign_call(f_', NAME, ').\n'), !.
function_wrapper(NAME, void, ARGTYPES) :-
	findall(_, member(_, ARGTYPES), VARS),
	length(VARS, N),
	gen(':- determinate(', NAME, '/', N, ').\n'),
	gen(NAME, '('),
	gen_list(VARS),
	gen(') :- foreign_call(f_', NAME, '('),
	gen_list(VARS),
	gen(')).\n'), !.
function_wrapper(NAME, RTYPE, []) :-
	functor(RTYPE, SF, _),
	memberchk(SF, [success, fail]),
	gen(':- determinate(', NAME, '/0).\n'),
	gen(NAME, ' :- foreign_call(f_', NAME, ').\n'), !.
function_wrapper(NAME, RTYPE, ARGTYPES) :-
	functor(RTYPE, SF, _),
	memberchk(SF, [success, fail]),
	findall(_, member(_, ARGTYPES), VARS),
	length(VARS, N),
	gen(':- determinate(', NAME, '/', N, ').\n'),
	gen(NAME, '('),
	gen_list(VARS),
	gen(') :- foreign_call(f_', NAME, '('),
	gen_list(VARS),
	gen(')).\n'), !.
function_wrapper(NAME, RTYPE, ARGTYPES) :-
	findall(_, member(_, ARGTYPES), VARS),
	length(VARS, N),
	N2 is N + 1,
	gen(':- determinate(', NAME, '/', N2, ').\n'),
	gen(NAME, '('),
	gen_list(VARS),
	(VARS == []; gen(',')),
	gen('R) :- foreign_call(f_', NAME, '('),
	gen_list(VARS),
	(VARS == []; gen(',')),
	gen('R)).\n'), !.


%% value conversion from C to Prolog

p_value(const(T), REF) :- p_value(T, REF).
p_value(unsigned(T), REF) :- p_value(T, REF).
p_value(pointer(const(T)), REF) :- p_value(pointer(T), REF).
p_value(pointer(char), REF) :- gen('(', REF, '?CSYMBOL(', REF, '):ZERO)').
p_value(pointer(_), REF) :- gen('(', REF, '?POINTER(', REF, '):ZERO)').
p_value('X', REF) :- gen(REF).
p_value(ITYPE, REF) :-
	memberchk(ITYPE, [char, int, long, short]),
	gen('word_to_fixnum(', REF, ')').
p_value(FTYPE, REF) :-
	memberchk(FTYPE, [float, double]),
	gen('FLONUM(', REF, ')').
p_value(TYPE, REF) :-
	gen('BLOB(&', REF, ',sizeof(', REF, '))').


%% value conversion from Prolog to C

c_value(const(T), REF) :- c_value(T, REF).
c_value(unsigned(T), REF) :- c_value(T, REF).
c_value(pointer(const(T)), REF) :- c_value(pointer(T), REF).
c_value(pointer(char), REF) :-
	gen('(', REF, '==ZERO?NULL:((char *)objdata(slot_ref(check_type_SYMBOL(', REF, '),0))))').
c_value(pointer(_), REF) :-
	gen('(', REF, '==ZERO?NULL:(void *)slot_ref(check_type_POINTER(', REF, '),0))').
c_value('X', REF) :- gen(REF).
c_value(ITYPE, REF) :-
	memberchk(ITYPE, [char, int, long, short]),
	gen('fixnum_to_word(check_fixnum(', REF, '))').
c_value(FTYPE, REF) :-
	memberchk(FTYPE, [float, double]),
	gen('(is_FIXNUM(', REF, ')?fixnum_to_float(', REF),
	gen('):flonum_to_float(check_type_FLONUM(', REF, ')))').
c_value(TYPE, REF) :-
	gen('(*((', TYPE, '*)objdata(', REF, ')))').


%% utilities

error(MSG, ARG) :-
	fwritef(user_error, '\n*** %d: %q\n', [MSG, ARG]),
	halt(1).

gen(T) :- display(T).
gen(T1, T2) :- gen(T1), gen(T2).
gen(T1, T2, T3) :- gen(T1, T2), gen(T3).
gen(T1, T2, T3, T4) :- gen(T1, T2, T3), gen(T4).
gen(T1, T2, T3, T4, T5) :- gen(T1, T2, T3, T4), gen(T5).

gen_list([]) :- !.
gen_list([X]) :- gen(X), !.
gen_list([X|R]) :- gen(X, ','), gen_list(R).

uppercase(C) :- C >= 65, C =< 90.
lowercase(C) :- C >= 97, C =< 122.
digit(C) :- C >= 48, C =< 57.

basename(FNAME, BNAME) :-
	(atom(FNAME) -> name(FNAME, LST); LST = FNAME),
	append(BNAME, [46|_], LST), !.
basename(FNAME, BNAME) :- name(FNAME, BNAME).

take_list([], _, []).
take_list(_, 0, []).
take_list([X|R], N, [X|R2]) :- N2 is N - 1, take_list(R, N2, R2).


%% toplevel

parse_arguments([]).
parse_arguments(['-h'|_]) :- usage(0).
parse_arguments(['-help'|_]) :- usage(0).
parse_arguments(['--help'|_]) :- usage(0).
parse_arguments(['-version'|_]) :- show_version_and_exit.
parse_arguments(['-i'|MORE]) :-
	recordz(include_source, yes),
	parse_arguments(MORE).
parse_arguments(['-q'|MORE]) :-
	recordz(silent, yes),
	parse_arguments(MORE).
parse_arguments(['-o', OUTNAME|MORE]) :-
	name(OUTNAME, OUTL),
	recordz(output_filename, OUTL),
	parse_arguments(MORE).
parse_arguments([FILENAME|MORE]) :-
	(\+recorded(source_file, _); usage(1)),
	recordz(source_file, FILENAME),
	parse_arguments(MORE).

usage(CODE) :-
	display('usage: pb [-version] [-h] [-q] [-i] [-o OUTPUTNAME] [FILENAME]\n'),
	halt(CODE).

show_version_and_exit :-
	current_prolog_flag(version, V),
	current_prolog_flag(prolog_title, T),
	current_prolog_flag(prolog_copyright, C),
	display(T), display(' version '), display(V), display(' - '),
	display(C), nl, halt.

process_input_file(FILENAME) :-
	see(FILENAME), read_string(all, INPUT), seen,
	process_input(INPUT).

process_input(INPUT) :-
	parse_definition(N, RN, RT, AT, INPUT, REST),
	!, process_input(REST).
process_input(INPUT) :-
	eof(INPUT), !.
process_input(INPUT) :-
	take_list(INPUT, 40, LST),
	append(LST, "...", LST2),
	name(STR, LST2),
	error('Invalid syntax', STR).

derive_filename(user, EXT, NEW) :- derive_filename(bind, EXT, NEW).
derive_filename(_, EXT, NEW) :-
	recorded(output_filename, OUTNAME),
	!, append(OUTNAME, EXT, NEW).
derive_filename(FNAME, EXT, NEW) :-
	basename(FNAME, BASENAME),
	append(BASENAME, EXT, NEW).

main :-
	command_line_arguments(ARGS),
	parse_arguments(ARGS),
	(recorded(source_file, SOURCEFILE); SOURCEFILE = user),
	process_input_file(SOURCEFILE),
	derive_filename(SOURCEFILE, ".h", HNAME),
	generate_header_file(HNAME),
	derive_filename(SOURCEFILE, ".pl", PLNAME),
	generate_wrapper_file(PLNAME, HNAME).
