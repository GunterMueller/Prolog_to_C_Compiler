%   File   : READ.PL
%   Author : D.H.D.Warren + Richard O'Keefe
%   Updated: 5 July 1984
%   Purpose: Read Prolog terms in Dec-10 syntax.
/*
    Modified by Alan Mycroft to regularise the functor modes.
    This is both easier to understand (there are no more '?'s),
    and also fixes bugs concerning the curious interaction of cut with
    the state of parameter instantiation.

    Since this file doesn't provide "metaread", it is considerably
    simplified.  The token list format has been changed somewhat, see
    the comments in the RDTOK file.

    I have added the rule X(...) -> apply(X,[...]) for Alan Mycroft.
*/


:- mode all_read(+),
	expect(-, +, -),
	read(+, +, -, -),
	read(+, +, +, -, -),
	read_args(+, -, -),
	read_list(+, -, -),
	peepop(+, -),
	prefix_is_atom(+, +),
	exprtl0(+, ?, +, -, -),
	cant_follow_expr(+, -),
	exprtl(+, +, ?, +, -, -),
	display_list(+),
	display_list(+, +),
	display_token(+).

:- determinate '$read2'/2,
	prefixop/3,
	postfixop/3,
	infixop/4,
	ambigop/6.


'$read1'(Answer) :- '$read1'(Answer, _).

%% this corresponds to ISO read/2
'$read2'(S, A) :-
	foreign_call(current_input_stream(OLD)),
	foreign_call(set_current_input_stream(S)),
	'$read1'(A),
	foreign_call(set_current_input_stream(OLD)).


%   read(?Answer, ?Variables)
%   reads a term from the current input stream and unifies it with
%   Answer.  Variables is bound to a list of [Atom=Variable] pairs.

'$read1'(Answer, Variables) :-
	repeat,
	    read_tokens(Tokens, Variables),
	    (   read(Tokens, 1200, Term, LeftOver), all_read(LeftOver)
	    ;   read_syntax_error(Tokens)
	    ),
	!,
	Answer = Term.


%   all_read(+Tokens)
%   checks that there are no unparsed tokens left over.

all_read([]) :- !.
all_read(S) :-
	read_syntax_error([operator,expected,after,expression], S).


%   expect(Token, TokensIn, TokensOut)
%   reads the next token, checking that it is the one expected, and
%   giving an error message if it is not.  It is used to look for
%   right brackets of various sorts, as they're all we can be sure of.

expect(Token, [Token|Rest], Rest) :- !.
expect(Token, S0, _) :-
	read_syntax_error([Token,or,operator,expected], S0).


%   I want to experiment with having the operator information held as
%   ordinary Prolog facts.  For the moment the following predicates
%   remain as interfaces to current_op.
%   prefixop(O -> Self, Rarg)
%   postfixop(O -> Larg, Self)
%   infixop(O -> Larg, Self, Rarg)


prefixop(Op, Prec, Prec) :-
	current_op(Prec, fy, Op), !.
prefixop(Op, Prec, Less) :-
	current_op(Prec, fx, Op), !,
	Less is Prec-1 .


postfixop(Op, Prec, Prec) :-
	current_op(Prec, yf, Op), !.
postfixop(Op, Less, Prec) :-
	current_op(Prec, xf, Op), !, Less is Prec-1 .


infixop(Op, Less, Prec, Less) :-
	current_op(Prec, xfx, Op), !, Less is Prec-1 .
infixop(Op, Less, Prec, Prec) :-
	current_op(Prec, xfy, Op), !, Less is Prec-1 .
infixop(Op, Prec, Prec, Less) :-
	current_op(Prec, yfx, Op), !, Less is Prec-1 .


ambigop(F, L1, O1, R1, L2, O2) :-
	postfixop(F, L2, O2),
	infixop(F, L1, O1, R1), !.


%   read(+TokenList, +Precedence, -Term, -LeftOver)
%   parses a Token List in a context of given Precedence,
%   returning a Term and the unread Left Over tokens.

read([Token|RestTokens], Precedence, Term, LeftOver) :-
	read(Token, RestTokens, Precedence, Term, LeftOver).
read([], _, _, _) :-
	read_syntax_error([expression,expected], []).


%   read(+Token, +RestTokens, +Precedence, -Term, -LeftOver)

read(var(Variable,_), ['('|S1], Precedence, Answer, S) :- !,
	read(S1, 999, Arg1, S2),
	read_args(S2, RestArgs, S3), !,
	exprtl0(S3, apply(Variable,[Arg1|RestArgs]), Precedence, Answer, S).

read(var(Variable,_), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Variable, Precedence, Answer, S).

read(atom(-), [integer(Integer)|S1], Precedence, Answer, S) :-
	Negative is -Integer, !,
	exprtl0(S1, Negative, Precedence, Answer, S).

read(atom(-), [number(Num)|S1], Precedence, Answer, S) :-
	Negative is -Num, !,
	exprtl0(S1, Negative, Precedence, Answer, S).

read(atom(Functor), ['('|S1], Precedence, Answer, S) :- !,
	read(S1, 999, Arg1, S2),
	read_args(S2, RestArgs, S3),
	Term =.. [Functor,Arg1|RestArgs], !,
	exprtl0(S3, Term, Precedence, Answer, S).

%% flw (allow prefix-op before punctutation)
read(atom(Functor), [Stop|S1], Precedence, Answer, S) :-
	memberchk(Stop, [',', ')', ']']), !,
	exprtl0([Stop|S1], Functor, Precedence, Answer, S).

read(atom(Functor), S0, Precedence, Answer, S) :-
	prefixop(Functor, Prec, Right), !,
	after_prefix_op(Functor, Prec, Right, S0, Precedence, Answer, S).

read(atom(Atom), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Atom, Precedence, Answer, S).

read(integer(Integer), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Integer, Precedence, Answer, S).

% flw (parse non-integer number)
read(number(Number), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Number, Precedence, Answer, S).

read('[', [']'|S1], Precedence, Answer, S) :- !,
	exprtl0(S1, [], Precedence, Answer, S).

read('[', S1, Precedence, Answer, S) :- !,
	read(S1, 999, Arg1, S2),
	read_list(S2, RestArgs, S3), !,
	exprtl0(S3, [Arg1|RestArgs], Precedence, Answer, S).

read('(', S1, Precedence, Answer, S) :- !,
	read(S1, 1200, Term, S2),
	expect(')', S2, S3), !,
	exprtl0(S3, Term, Precedence, Answer, S).

read(' (', S1, Precedence, Answer, S) :- !,
	read(S1, 1200, Term, S2),
	expect(')', S2, S3), !,
	exprtl0(S3, Term, Precedence, Answer, S).

read('{', ['}'|S1], Precedence, Answer, S) :- !,
	exprtl0(S1, '{}', Precedence, Answer, S).

read('{', S1, Precedence, Answer, S) :- !,
	read(S1, 1200, Term, S2),
	expect('}', S2, S3), !,
	exprtl0(S3, '{}'(Term), Precedence, Answer, S).

read(string(List), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, List, Precedence, Answer, S).

read(Token, S0, _, _, _) :-
	read_syntax_error([Token,cannot,start,an,expression], S0).


%   read_args(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ')' and returns a list of terms.

read_args([','|S1], [Term|Rest], S) :- !,
	read(S1, 999, Term, S2), !,
	read_args(S2, Rest, S).
read_args([')'|S], [], S) :- !.
read_args(S, _, _) :-
	read_syntax_error([', or )',expected,in,arguments], S).


%   read_list(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ['|' expr(999)] ']' and returns a list 
%   of terms.

read_list([','|S1], [Term|Rest], S) :- !,
	read(S1, 999, Term, S2), !,
	read_list(S2, Rest, S).
read_list(['|'|S1], Rest, S) :- !,
	read(S1, 999, Rest, S2), !,
	expect(']', S2, S).
read_list([']'|S], [], S) :- !.
read_list(S, _, _) :-
	read_syntax_error([', | or ]',expected,in,list], S).


%   after_prefix_op(+Op, +Prec, +ArgPrec, +Rest, +Precedence, 
%       -Ans, -LeftOver)

after_prefix_op(Op, Oprec, _, S0, Precedence, _, _) :-
	Precedence < Oprec, !,
	read_syntax_error([prefix,operator,Op,in,context,
		with,precedence,Precedence], S0).

after_prefix_op(Op, Oprec, _, S0, Precedence, Answer, S) :-
	peepop(S0, S1),
	prefix_is_atom(S1, Oprec), % can't cut but would like to
	exprtl(S1, Oprec, Op, Precedence, Answer, S).

after_prefix_op(Op, Oprec, Aprec, S1, Precedence, Answer, S) :-
	read(S1, Aprec, Arg, S2),
	Term =.. [Op,Arg], !,
	exprtl(S2, Oprec, Term, Precedence, Answer, S).


%   The next clause fixes a bug concerning "mop dop(1,2)" where
%   mop is monadic and dop dyadic with higher Prolog priority.

peepop([atom(F),'('|S1], [atom(F),'('|S1]) :- !.
peepop([atom(F)|S1], [infixop(F,L,P,R)|S1]) :- infixop(F, L, P, R).
peepop([atom(F)|S1], [postfixop(F,L,P)|S1]) :- postfixop(F, L, P).
peepop(S0, S0).


%   prefix_is_atom(+TokenList, +Precedence)
%   is true when the right context TokenList of a prefix operator
%   of result precedence Precedence forces it to be treated as an
%   atom, e.g. (- = X), p(-), [+], and so on.

prefix_is_atom([Token|_], Precedence) :-
	prefix_is_atom(Token, Precedence).

prefix_is_atom(infixop(_,L,_,_), P) :- L >= P.
prefix_is_atom(postfixop(_,L,_), P) :- L >= P.
prefix_is_atom(')', _).
prefix_is_atom(']', _).
prefix_is_atom('}', _).
prefix_is_atom('|', P) :- 1100 >= P.
prefix_is_atom(',', P) :- 1000 >= P.
prefix_is_atom([],  _).


%   exprtl0(+Tokens, +Term, +Prec, -Answer, -LeftOver)
%   is called by read/4 after it has read a primary (the Term).
%   It checks for following postfix or infix operators.

exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	ambigop(F, L1, O1, R1, L2, O2), !,
	(   exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, 
		Answer, S)
	;   exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, 
		Answer, S)
	).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	infixop(F, L1, O1, R1), !,
	exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, 
	    Answer, S).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	postfixop(F, L2, O2), !,
	exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, 
	    Answer, S).

exprtl0([','|S1], Term, Precedence, Answer, S) :-
	Precedence >= 1000, !,
	read(S1, 1000, Next, S2), !,
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).

exprtl0(['|'|S1], Term, Precedence, Answer, S) :-
	Precedence >= 1100, !,
	read(S1, 1100, Next, S2), !,
	exprtl(S2, 1100, (Term;Next), Precedence, Answer, S).

exprtl0([Thing|S1], _, _, _, _) :-
	cant_follow_expr(Thing, Culprit), !,
	read_syntax_error([Culprit,follows,expression], [Thing|S1]).

exprtl0(S, Term, _, Term, S).


cant_follow_expr(atom(_),	atom).
cant_follow_expr(var(_,_),	variable).
cant_follow_expr(integer(_),	integer).
cant_follow_expr(string(_),	string).
cant_follow_expr(' (',		bracket).
cant_follow_expr('(',		bracket).
cant_follow_expr('[',		bracket).
cant_follow_expr('{',		bracket).



exprtl([infixop(F,L,O,R)|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= O, C =< L, !,
	read(S1, R, Other, S2),
	Expr =.. [F,Term,Other], /*!,*/
	exprtl(S2, O, Expr, Precedence, Answer, S).

exprtl([postfixop(F,L,O)|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= O, C =< L, !,
	Expr =.. [F,Term],
	peepop(S1, S2),
	exprtl(S2, O, Expr, Precedence, Answer, S).

exprtl([','|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 1000, C < 1000, !,
	read(S1, 1000, Next, S2), /*!,*/
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).

exprtl(['|'|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 1100, C < 1100, !,
	read(S1, 1100, Next, S2), /*!,*/
	exprtl(S2, 1100, (Term;Next), Precedence, Answer, S).

exprtl(S, _, Term, _, Term, S).


%   This business of syntax errors is tricky.  When an error is 
%   detected, we have to write out a message.  We also have to note 
%   how far it was to the end of the input, and for this we are 
%   obliged to use the data-base.  Then we fail all the way back to 
%   read(), and that prints the input list with a marker where the 
%   error was noticed.  If subgoal_of were available in compiled code 
%   we could use that to find the input list without hacking the 
%   data base.  The really hairy thing is that the original code 
%   noted a possible error and backtracked on, so that what looked 
%   at first sight like an error sometimes turned out to be a wrong 
%   decision by the parser.  This version of the parser makes
%   fewer wrong decisions, and my goal was to get it to do no
%   backtracking at all.  This goal has not yet been met, and it 
%   will still occasionally report an error message and then decide 
%   that it is happy with the input after all.  Sorry about that.


read_syntax_error(Message, List) :-
	display(user_error, '\n**'),
	display_list(Message),
	length(List, Length),
	recorda(syntax_error, length(Length)), !,
	fail.

display_list([Head|Tail]) :-
	put(user_error, 32),
	display_token(Head), !,
	display_list(Tail).
display_list([]) :-
	nl(user_error).

read_syntax_error(List) :-
	recorded(syntax_error, length(AfterError), Ref),
	erase(Ref),
	length(List, Length),
	BeforeError is Length-AfterError,
	telling(OLD), current_error_output(ERR), tell(ERR),
	display_list(List, BeforeError), !,
	tell(OLD),
	throw(syntax_error).

display_list(X, 0) :-
	display(user_error, '<<here>> '), !,
	display_list(X, 99999).
display_list([Head|Tail], BeforeError) :-
	display_token(Head),
	put(user_error, 32),
	Left is BeforeError-1, !,
	display_list(Tail, Left).
display_list([], _) :-
	nl(user_error).

display_token(atom(X))	  :- !,	display(user_error, X).
display_token(var(_,X))	  :- !,	display(user_error, X).
display_token(integer(X)) :- !,	display(user_error, X).
display_token(string(X))  :- !,	display(user_error, X).
display_token(X)	  :-	display(user_error, X).
