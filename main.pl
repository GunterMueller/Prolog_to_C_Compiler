%%% toplevel / driver for compiler
%
% - supported options:
%
%   -version                show version and exit
%   -v                      show clauses are they are compiled
%   -h  -help  --help       show short usage description
%   -i                      show intermediate code instead of generating C
%   -o FILENAME             override output file name (default: <SOURCE_FILE>.c)
%   -q                      disable any output
%   -compress-facts         compress blocks of facts with ground arguments
%   -xref                   write cross-referencing information to stdout
%   -xrefall                write xref information for all code
%   -n                      ignore PC_LIBRARY_DIR
%
% - The content of PC_INCLUDE_PATH are prepended to the default include-path.

:- initialization(main).

main :-
	command_line_arguments(ARGS),
	compile(ARGS),
	halt.

compile(ARGS) :-
	set_library_path,
	parse_arguments(ARGS),
	(recorded(source_file, FILE); usage(1)),
	compile_file(FILE).

set_library_path :-
	(getenv('PC_LIBRARY_DIR', DIR); default_setting(library_dir, DIR)),
	recordz(library_dir, DIR).

parse_arguments([]).
parse_arguments(['-o', OFILE|MORE]) :-
	recorda(output_file, OFILE),
	parse_arguments(MORE).
parse_arguments(['-xref'|MORE]) :-
	recordz(xref_mode, yes),
	parse_arguments(MORE).
parse_arguments(['-xrefall'|MORE]) :-
	recordz(xref_mode, all),
	parse_arguments(MORE).
parse_arguments(['-i'|MORE]) :-
	recorda(show_intermediate_code, yes),
	parse_arguments(MORE).
parse_arguments(['-v'|MORE]) :-
	recorda(show_compiled_clauses, yes),
	parse_arguments(MORE).
parse_arguments(['-n'|MORE]) :-
	recorded(library_dir, _, REF), erase(REF),
	default_setting(library_dir, DIR),
	recordz(library_dir, DIR),
	parse_arguments(MORE).
parse_arguments(['-q'|MORE]) :-
	recorda(silent, yes),
	parse_arguments(MORE).
parse_arguments(['-compress-facts'|MORE]) :-
	recordz(compress_facts, yes),
	parse_arguments(MORE).
parse_arguments(['-h'|_]) :- usage(0).
parse_arguments(['-help'|_]) :- usage(0).
parse_arguments(['--help'|_]) :- usage(0).
parse_arguments(['-version'|_]) :- show_version_and_exit.
parse_arguments([INFILE|MORE]) :-
	name(INFILE, IFL),
	IFL \= [45|_],		% 0'-
	file_name_string(IFL, OFL),
	append(OFL, ".c", OFL2),
	name(OUTFILE, OFL2),
	recorda(source_file, INFILE),
	(recorded(output_file, _) -> true; recorda(output_file, OUTFILE)),
	parse_arguments(MORE).
parse_arguments(_) :- usage(1).

usage(STATUS) :-
	gen('usage: pc [-version] [-h] [-v] [-o FILENAME] [-i]',
	    ' [-xref] [-xrefall] [-compress-facts] [FILENAME]\n'),
	halt(STATUS).
