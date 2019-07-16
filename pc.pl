%%% Wrapper for compiling compiler with itself


:- include('settings.pl').
:- include('support.pl').
:- include('state.pl').
:- include('terms.pl').
:- include('index.pl').
:- include('lib/dcg.pl').
:- include('builtin.pl').
:- include('process.pl').
:- include('compile.pl').
:- include('assemble.pl').
:- include('xref.pl').
:- include('dce.pl').

show_version_and_exit :-
	current_prolog_flag(version, V),
	current_prolog_flag(prolog_title, T),
	current_prolog_flag(prolog_copyright, C),
	display(T), display(' version '), display(V), display(' - '),
	display(C), nl, halt.

skip_shebang :-
	(\+peek_char('#'); read_line(_)).

:- include('main.pl').
