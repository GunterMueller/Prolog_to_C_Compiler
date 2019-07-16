
% PVR 20/07/92: Added entry declaration for move/5 (it is used in a bagof).

% ------------------------------------------------------------------------------
% Chess -- Mar. 1, 1987 	Mike Carlton
%
% Standard rules of chess apply with the following exceptions:
%	en passant captures are not allowed,
%	pawns are promoted to queens only,
%	draw by repetition or no capture are not allowed,
%	kings may castle out of or through check (but not into).
%
% Files are numbered a to h, ranks are numbered 1 to 8,
% and white is assumed to play from the rank 1 side.
% The program always plays black.
%
% Positions are specified with the structure: File-Rank. 
% The board is a list containing two state structures of the form:
%	state(white, WhiteKing, WhiteKingRook, WhiteQueenRook), 
%	state(black, BlackKing, BlackKingRook, BlackQueenRook),
% followed by a list of the piece positions of the form: 
%	piece(File-Rank, Color, Type),
% where the state variables will be bound to an atom once the
% corresponding piece has been moved.
% A move is stored internally as: move(From_File-From_Rank, To_File-To_Rank).
%
% Commands available:
%	Move:  entered in the form FRFR (where F is in a..h and R is in 1..8)
%	board: prints the current board
%	quit:  quits the game
%	move(Move): makes any move and does not respond
%
% The program has a very small book, it will play much better if you stick
% to the book opening!  The opening it plays is:
%	White   Black
%       -----   -----
%	e2e4	e7e5
%	g1f3	b8c6
%	f1b5	a7a6

main :-
    	init_board(InBoard),
	test_play(InBoard, _),
	fail.
main.

%% test_play -->
%% 	execute_command(e2e4),
%% 	!,
%% 	execute_command(d2d3),
%% 	!,
%% 	execute_command(quit).

test_play(S0,S4) :-
	execute_command(e2e4,S0,S1),!,
	execute_command(d2d3,S1,S2),!,
	execute_command(c1e3,S2,S3),  %% added
	!,
	execute_command(quit,S3,S4).

play(Board) :-
	get_command(Command),
	execute_command(Command, Board, NewBoard), !,
	play(NewBoard).
 
get_command(Command) :-
	nl, write('-> '),
	read(Command), !.
get_command(Command) :-
	get_command(Command).
 
execute_command(bad, Board, Board) :- 		% To catch vars
     	write('What?').
execute_command(set1, _, Board) :-			% DEBUG
	Board = ([state(white, king_moved, king_rook_moved, queen_rook_moved), 
	   	  state(black, king_moved, king_rook_moved, queen_rook_moved),
		  piece(a-1, white, king), piece(b-1, white, queen),
		  piece(c-1, black, king)]).
execute_command(set2, _, Board) :-			% DEBUG
	Board = ([state(white, king_moved, king_rook_moved, queen_rook_moved), 
	   	  state(black, king_moved, king_rook_moved, queen_rook_moved),
		  piece(a-2, white, king), piece(b-1, white, queen),
		  piece(h-8, black, king), piece(b-8, black, rook),
		  piece(c-3, black, night)]).
execute_command(clear, _, Board) :-			% DEBUG
	Board = ([state(white, king_moved, king_rook_moved, queen_rook_moved), 
	   	  state(black, king_moved, king_rook_moved, queen_rook_moved)
		 ]).
execute_command(piece(S,C,T), Board, [piece(F-R,C,T)|Board]) :-  % DEBUG
	parse_square(S, F-R),
	member(F, [a, b, c, d, e, f, g, h]),
	member(R, [1, 2, 3, 4, 5, 6, 7, 8]),
	member(C, [white, black]),
	member(T, [king, queen, rook, night, bishop, pawn]).
% execute_command(stats, Board, Board) :-			% DEBUG
%      	statistics.
execute_command(ply(N), Board, Board) :-		% DEBUG
     	abolish(ply_depth/1),
	assertz(ply_depth(N)).
execute_command(rate, Board, Board) :-			% DEBUG
	computer(ComputerColor),
	evaluate(Board, ComputerColor, Val), 
	write(' Rating: '), write(Val), nl.
execute_command(rawboard, Board, Board) :-		% DEBUG
     	write(Board), nl.
execute_command(move(Move), Board, NewBoard) :-	
     	parse_move(Move, From, To),
     	make_move(Board, From, To, NewBoard).  
execute_command(quit, _, _) :-
    	write('Goodbye.'), nl, !, fail.
execute_command(board, Board, Board) :-
     	print_board(Board).
execute_command(Move, Board, NewBoard) :-
     	parse_move(Move, From, To),
     	respond_to(Board, From, To, NewBoard).  
execute_command(_, Board, Board) :-
     	write('What?').


% ------------------------------------------------------------------------------
% Parameters

% ply_depth(2).			% Depth of alpha-beta search
ply_depth(3).

player(white).
computer(black).

value(king, 10000) :- ! .
value(queen,  900) :- ! .
value(rook,   500) :- ! .
value(night,  300) :- ! .
value(bishop, 300) :- ! .
value(pawn,   100) :- ! .


% ------------------------------------------------------------------------------
% Chess procedures                                                  

respond_to(Board, From, To, OutBoard) :-
	player(PlayerColor),
	legal_move(Board, PlayerColor, From, To),
	write('Working...'), nl,
      	make_move(Board, From, To, NewBoard), 
% statistics,
	select_move(NewBoard, ComputerFrom, ComputerTo, Rating),
% statistics,
	finish_move(NewBoard, ComputerFrom, ComputerTo, Rating, OutBoard),
	% true, !.
	garbage_collect, !.	% gc

respond_to(Board, _, _, Board) :-
	write('Illegal move.').

finish_move(Board, ComputerFrom, ComputerTo, -32000, Board) :-
	in_check(Board, black),
	write('Checkmate, you win.'), nl,
	print_board(Board).
finish_move(Board, ComputerFrom, ComputerTo, -32000, Board) :-
	write('Stalemate.'), nl,
	print_board(Board).
finish_move(NewBoard, ComputerFrom, ComputerTo, Rating, OutBoard) :-
	make_move(NewBoard, ComputerFrom, ComputerTo, OutBoard),
	report_move(OutBoard, ComputerFrom, ComputerTo, Rating).

select_move(Board, From, To, book) :-		% Use book 
	book(Board, From, To).			
select_move(Board, From, To, Rating) :-		% time for ALPHA-BETA
	computer(ComputerColor),
	ply_depth(Depth),
	alpha_beta(Board, ComputerColor, Depth, -32000, 32000, move(From, To), Rating).

alpha_beta(Board, Color, 0, Alpha, Beta, BestMove, MoveVal) :-
	computer(ComputerColor),
	evaluate(Board, ComputerColor, MoveVal). 
	% write('.').
	% write(' val: '), write(MoveVal), nl.
alpha_beta(Board, Color, Depth, Alpha, Beta, BestMove, MoveVal) :-
	collect_moves(Board, Color, MoveList), !,
	find_best(Board, Color, MoveList, Depth, Alpha, Beta, BestMove, MoveVal).

find_best(Board, Color, [move(From, To)|Moves], Depth, Alpha, Beta, 
	  BestMove, BestVal) :-
	make_move(Board, From, To, NewBoard),
%	write('*'),
	% write(Color),write(' '),write(From),write(' '),write(To),
	% write(' '),write(Alpha), write(' '),write(Beta),write(' '),	
	NextDepth is Depth - 1,
	opposite(Color, OppositeColor),
	alpha_beta(NewBoard, OppositeColor, NextDepth, Alpha, Beta, _, Val),
	sufficient(Board, Color, Moves, Depth, Alpha, Beta, move(From,To), Val, BestMove, BestVal).

sufficient(Board, Color, [], Depth, Alpha, Beta, Move, Val, Move, Val).	
sufficient(Board, Color, Moves, Depth, Alpha, Beta, Move, Val, Move, Val) :-
	player(Color),
	Val < Alpha,
	write('x').
	% write('       Pruning for player- val:'), write(Val),
	% write(' '),write(Alpha),write(' '),write(Beta),nl. 
sufficient(Board, Color, Moves, Depth, Alpha, Beta, Move, Val, Move, Val) :-
	computer(Color), 		
	Val > Beta,
	true. % write('X'),nl.
	% write('       Pruning for computer- val:'), write(Val),
	% write(' '),write(Alpha),write(' '),write(Beta),nl. 
sufficient(Board, Color, Moves, Depth, Alpha, Beta, Move, Val, 
	   BestMove, BestVal) :-
	new_bounds(Color, Alpha, Beta, Val, NewAlpha, NewBeta),
	find_best(Board, Color, Moves, Depth, NewAlpha, NewBeta, Move1, Val1),
	better_of(Color, Move, Val, Move1, Val1, BestMove, BestVal).

new_bounds(Color, Alpha, Beta, Val, Val, Beta) :-
	computer(Color),			% Maximizing
	Val > Alpha.
new_bounds(Color, Alpha, Beta, Val, Alpha, Val) :-
	player(Color), 				% Minimizing
	Val < Beta.
new_bounds(_, Alpha, Beta, _, Alpha, Beta).

better_of(Color, Move, Val, Move1, Val1, Move, Val) :-
	player(Color), 				% Minimizing
	Val < Val1.
better_of(Color, Move, Val, Move1, Val1, Move, Val) :-
	computer(Color),			% Maximizing
	Val > Val1.
better_of(_, _, _, Move1, Val1, Move1, Val1).

evaluate(Board, Color, -32000) :-
	in_check(Board, Color).
evaluate(Board, Color, Rating) :-
	opposite(Color, OppositeColor),
	strength(Board, Color, OppositeColor, Rating).

strength([], _, _, 0).
strength([state(_, _, _, _)|Board], Color, OppositeColor, Strength) :-
	strength(Board, Color, OppositeColor, Strength).
strength([piece(_, Color, Type)|Board], Color, OppositeColor, Strength) :-
	value(Type, Value),
	strength(Board, Color, OppositeColor, PartialStrength),
	Strength is PartialStrength + Value.
strength([piece(_, OppositeColor, Type)|Board], Color, OppositeColor, 
	 Strength) :-
	value(Type, Value),
	strength(Board, Color, OppositeColor, PartialStrength),
	Strength is PartialStrength - Value.

legal_move(Board, Color, From, To) :-
	move(Board, From, To, Color, Piece),
	make_move(Board, From, To, NewBoard), !,
	\+ in_check(NewBoard, Color).

in_check(Board, Color) :-
	member(piece(KingSquare, Color, king), Board),
	opposite(Color, OppositeColor),
	move(Board, _, KingSquare, OppositeColor, _).

%:- entry((move(Board,From,To,Color,Piece) :- nonvar(Board), atom(Color))).

collect_moves(Board, Color, Moves) :-
%% 	write('..............'),nl,
%% 	statistics,
%% 	(ground(Board-Color) ->
%% 	    true
%% 	;
%% 	    write(Board-Color),nl
%% 	),
	bagof(move(From, To), Piece^move(Board,From,To,Color,Piece), Moves).

make_move(Board, From, To, OutBoard) :-
	make_move(Board, From, To, Color, Type, NewBoard),	  
	update_state(NewBoard, From, Color, Type),
	check_castling(NewBoard, From, To, Color, Type, OutBoard).

make_move([], From, File-8, white, pawn, [piece(File-8, white, queen)]).
make_move([], From, File-1, black, pawn, [piece(File-1, black, queen)]).
make_move([], From, To, Color, Type, [piece(To, Color, Type)]).     % Add To sq.
make_move([piece(From, Color, Type)|Board], From, To, Color, Type, OutBoard) :-
	make_move(Board, From, To, Color, Type, OutBoard).          % Skip From
make_move([piece(To, _, _)|Board], From, To, Color, Type, OutBoard) :-
	make_move(Board, From, To, Color, Type, OutBoard).          % Skip To sq
make_move([Piece|Board], From, To, Color, Type, [Piece|OutBoard]) :-
	make_move(Board, From, To, Color, Type, OutBoard).	    % Copy

check_castling(Board, e-Rank, g-Rank, Color, king, OutBoard) :- % King side
	make_move(Board, h-Rank, f-Rank, OutBoard).		% castling
check_castling(Board, e-Rank, c-Rank, Color, king, OutBoard) :- % Queen side
	make_move(Board, a-Rank, d-Rank, OutBoard).		% castling
check_castling(Board, _, _, _, _, Board).

parse_square(Square, File-Rank) :-
     	name(Square, [F,R]),
     	name(File, [F]),
     	myname(Rank, [R]),
     	on_board(File-Rank).
 
parse_move(Move, From_File-From_Rank, To_File-To_Rank) :-
     	name(Move, [FF,FR,TF,TR]),
     	name(From_File, [FF]),
     	myname(From_Rank, [FR]),
     	name(To_File, [TF]),
     	myname(To_Rank, [TR]),
     	on_board(From_File-From_Rank),
     	on_board(To_File-To_Rank).
 
on_board(File-Rank) :-
	member(File, [a, b, c, d, e, f, g, h]),
	member(Rank, [1, 2, 3, 4, 5, 6, 7, 8]).

not_moved(Board, Color, king) :-
	member(state(Color, King, _, _), Board), !,
	var(King).
not_moved(Board, Color, king, rook) :-
	member(state(Color, _, KingRook, _), Board), !,
	var(KingRook).
not_moved(Board, Color, queen, rook) :-
	member(state(Color, _, _, QueenRook), Board), !,
	var(QueenRook).

update_state(Board, From, Color, king) :-		% Was king moved?
	member(state(Color, king_moved, _, _), Board).
update_state(Board, h-Rank, Color, rook) :-		% Was king rook moved?
	member(state(Color, _, king_rook_moved, _), Board).
update_state(Board, a-Rank, Color, rook) :-		% Was queen rook moved?
	member(state(Color, _, _, queen_rook_moved), Board).
update_state(_, _, _, _).				% Else, ignore


% ------------------------------------------------------------------------------
% Printing utilities 

report_move(Board, From_File-From_Rank, To_File-To_Rank, Rating) :-
	nl,
	write('My move: '), 
	write(From_File), 
	write(From_Rank),
	write(To_File),
	write(To_Rank),
	write(', Rating: '),
	write(Rating), nl,
	print_board(Board).

%% print_board(Board) :-
%% 	!.

print_board(Board) :-
	print_rank(Board, 8),
	print_dashes, nl,
	print_letters, nl, !.
 
print_rank(Board, 1) :-
	print_dashes, nl,
	write(1),
	write(' | '),
	print_squares(Board, a, 1).
print_rank(Board, Rank) :-
	print_dashes, nl,
	write(Rank),
	write(' | '),
	print_squares(Board, a, Rank),
	minus_one(Rank, NextRank),
%  	Next_Rank is Rank - 1,  % the void variable
	NextRank is Rank - 1,
	print_rank(Board, NextRank).
 
print_squares(Board, h, Rank) :-
	print_a_square(Board, h, Rank),
	write(' |'),
	nl.
print_squares(Board, File, Rank) :-
	print_a_square(Board, File, Rank),
	write(' | '),
	plus_one(File, NextFile),
	print_squares(Board, NextFile, Rank).
 
print_a_square(Board, File, Rank) :-
	member(piece(File-Rank, Color, Type), Board),
	(Color = white -> write(' ') ; write('*')),
	name(Type, [T|_]),
	put(T).
print_a_square(Board, File, Rank) :-
	write('  ').
 
print_dashes :-
	write('  +----+----+----+----+----+----+----+----+').
 
print_letters :-
	write('    a    b    c    d    e    f    g    h').

myname(X,[Y]) :-
	myname2(Y,X), !.

myname2(48,0).
myname2(49,1).
myname2(50,2).
myname2(51,3).
myname2(52,4).
myname2(53,5).
myname2(54,6).
myname2(55,7).
myname2(56,8).
myname2(57,9).


% ------------------------------------------------------------------------------
% Initialization stuff

init_board( [
 	state(white, WhiteKing, WhiteKingRook, WhiteQueenRook), 
	state(black, BlackKing, BlackKingRook, BlackQueenRook),
	piece(a-8, black, rook  ), piece(b-8, black, night ),
	piece(c-8, black, bishop), piece(d-8, black, queen ),
	piece(e-8, black, king  ), piece(f-8, black, bishop),
	piece(g-8, black, night ), piece(h-8, black, rook  ),
	piece(a-7, black, pawn  ), piece(b-7, black, pawn  ),
	piece(c-7, black, pawn  ), piece(d-7, black, pawn  ),
	piece(e-7, black, pawn  ), piece(f-7, black, pawn  ),
	piece(g-7, black, pawn  ), piece(h-7, black, pawn  ),
	piece(a-1, white, rook  ), piece(b-1, white, night ),
	piece(c-1, white, bishop), piece(d-1, white, queen ),
	piece(e-1, white, king  ), piece(f-1, white, bishop),
	piece(g-1, white, night ), piece(h-1, white, rook  ),
	piece(a-2, white, pawn  ), piece(b-2, white, pawn  ),
	piece(c-2, white, pawn  ), piece(d-2, white, pawn  ),
	piece(e-2, white, pawn  ), piece(f-2, white, pawn  ),
	piece(g-2, white, pawn  ), piece(h-2, white, pawn  ) ]).


% ------------------------------------------------------------------------------
% Valid move checkers and generators 

move(Board, F_File-F_Rank, T_File-T_Rank, Color, Piece) :-
	occupied_by(Board, F_File-F_Rank, Color, Piece),
	can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, Piece),
	\+ occupied_by(Board, T_File-T_Rank, Color, _).

can_move(Board, File-F_Rank, File-T_Rank, white, pawn) :-     	% White pawn 
	plus_one(F_Rank, T_Rank),				% move
	\+ occupied_by(Board, File-T_Rank, black, _).
can_move(Board, File-2, File-4, white, pawn) :-  	      
	\+ occupied_by(Board, File-3, black, _),
	\+ occupied_by(Board, File-4, black, _).
can_move(Board, F_File-F_Rank, T_File-T_Rank, white, pawn) :- 	% White pawn 
	plus_one(F_File, T_File),				% capture
     	plus_one(F_Rank, T_Rank),
	occupied_by(Board, T_File-T_Rank, black, _).
can_move(Board, F_File-F_Rank, T_File-T_Rank, white, pawn) :-	
	minus_one(F_File, T_File),			
     	plus_one(F_Rank, T_Rank),
	occupied_by(Board, T_File-T_Rank, black, _).

can_move(Board, File-F_Rank, File-T_Rank, black, pawn) :-	% Black pawn 
	minus_one(F_Rank, T_Rank),				% move
	\+ occupied_by(Board, File-T_Rank, white, _).
can_move(Board, File-7, File-5, black, pawn) :-
	\+ occupied_by(Board, File-6, white, _),
	\+ occupied_by(Board, File-5, white, _).
can_move(Board, F_File-F_Rank, T_File-T_Rank, black, pawn) :-	% Black pawn 
	minus_one(F_File, T_File),				% capture
     	minus_one(F_Rank, T_Rank),
	occupied_by(Board, T_File-T_Rank, white, _).
can_move(Board, F_File-F_Rank, T_File-T_Rank, black, pawn) :-
	plus_one(F_File, T_File),
     	minus_one(F_Rank, T_Rank),
	occupied_by(Board, T_File-T_Rank, white, _).

can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, night) :-	% Knight move 
	plus_one(F_File, T_File), plus_two(F_Rank, T_Rank).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, night) :-
	plus_one(F_File, T_File), minus_two(F_Rank, T_Rank).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, night) :-
	minus_one(F_File, T_File), plus_two(F_Rank, T_Rank).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, night) :-
 	minus_one(F_File, T_File), minus_two(F_Rank, T_Rank).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, night) :-
	plus_two(F_File, T_File), plus_one(F_Rank, T_Rank).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, night) :-
	plus_two(F_File, T_File), minus_one(F_Rank, T_Rank).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, night) :-
	minus_two(F_File, T_File), plus_one(F_Rank, T_Rank).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, night) :-
	minus_two(F_File, T_File), minus_one(F_Rank, T_Rank).

can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, bishop) :- % Bishop move 
	can_step(Board,  1,  1, F_File-F_Rank, T_File-T_Rank).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, bishop) :-	
	can_step(Board,  1, -1, F_File-F_Rank, T_File-T_Rank).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, bishop) :-	
	can_step(Board, -1,  1, F_File-F_Rank, T_File-T_Rank).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, bishop) :-	
	can_step(Board, -1, -1, F_File-F_Rank, T_File-T_Rank).

can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, rook) :-	% Rook move 
	can_step(Board,  1,  0, F_File-F_Rank, T_File-T_Rank).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, rook) :-	
	can_step(Board, -1,  0, F_File-F_Rank, T_File-T_Rank).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, rook) :-	
	can_step(Board,  0,  1, F_File-F_Rank, T_File-T_Rank).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, rook) :-	
	can_step(Board,  0, -1, F_File-F_Rank, T_File-T_Rank).

can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, queen) :-	% Queen move 
	can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, bishop).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, queen) :-	
	can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, rook).

can_move(Board, File-F_Rank, File-T_Rank, Color, king) :-	% King move 
	plus_one(F_Rank, T_Rank).
can_move(Board, File-F_Rank, File-T_Rank, Color, king) :-	
	minus_one(F_Rank, T_Rank).
can_move(Board, F_File-Rank, T_File-Rank, Color, king) :-	% King move 
	plus_one(F_File, T_File).
can_move(Board, F_File-Rank, T_File-Rank, Color, king) :-	
	minus_one(F_File, T_File).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, king) :-	
	plus_one(F_File, T_File),
	minus_one(F_Rank, T_Rank).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, king) :-	
	plus_one(F_File, T_File),
	plus_one(F_Rank, T_Rank).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, king) :-	
	minus_one(F_File, T_File),
	plus_one(F_Rank, T_Rank).
can_move(Board, F_File-F_Rank, T_File-T_Rank, Color, king) :-	
	minus_one(F_File, T_File),
	minus_one(F_Rank, T_Rank).

can_move(Board, e-Rank, g-Rank, Color, king) :-			% King side 
     	not_moved(Board, Color, king),				% castle
     	not_moved(Board, Color, king, rook),
	\+ occupied_by(Board, f-Rank, _, _).
can_move(Board, e-Rank, g-Rank, Color, king) :-			% Queen side 
     	not_moved(Board, Color, king),				% castle
     	not_moved(Board, Color, queen, rook),
	\+ occupied_by(Board, d-Rank, _, _),
	\+ occupied_by(Board, b-Rank, _, _).
 
can_step(Board,  1,  1, F_File-F_Rank, T_File-T_Rank) :-	% Step moves
	plus_one(F_File, T_File),
	plus_one(F_Rank, T_Rank).
can_step(Board,  1,  1, F_File-F_Rank, T_File-T_Rank) :-
	plus_one(F_File, I_File),
	plus_one(F_Rank, I_Rank),
	\+ occupied_by(Board, I_File-I_Rank, _, _),
	can_step(Board,  1,  1, I_File-I_Rank, T_File-T_Rank).
can_step(Board,  1, 0, F_File-Rank, T_File-Rank) :-
	plus_one(F_File, T_File).
can_step(Board,  1, 0, F_File-Rank, T_File-Rank) :-
	plus_one(F_File, I_File),
	\+ occupied_by(Board, I_File-Rank, _, _),
	can_step(Board,  1, 0, I_File-Rank, T_File-Rank).
can_step(Board,  1, -1, F_File-F_Rank, T_File-T_Rank) :-
	plus_one(F_File, T_File),
	minus_one(F_Rank, T_Rank).
can_step(Board,  1, -1, F_File-F_Rank, T_File-T_Rank) :-
	plus_one(F_File, I_File),
	minus_one(F_Rank, I_Rank),
	\+ occupied_by(Board, I_File-I_Rank, _, _),
	can_step(Board,  1, -1, I_File-I_Rank, T_File-T_Rank).
can_step(Board,  0,  1, File-F_Rank, File-T_Rank) :-
	plus_one(F_Rank, T_Rank).
can_step(Board,  0,  1, File-F_Rank, File-T_Rank) :-
	plus_one(F_Rank, I_Rank),
	\+ occupied_by(Board, File-I_Rank, _, _),
	can_step(Board,  0,  1, File-I_Rank, File-T_Rank).
can_step(Board,  0, -1, File-F_Rank, File-T_Rank) :-
	minus_one(F_Rank, T_Rank).
can_step(Board,  0, -1, File-F_Rank, File-T_Rank) :-
	minus_one(F_Rank, I_Rank),
	\+ occupied_by(Board, File-I_Rank, _, _),
	can_step(Board,  0, -1, File-I_Rank, File-T_Rank).
can_step(Board, -1,  1, F_File-F_Rank, T_File-T_Rank) :-
	minus_one(F_File, T_File),
	plus_one(F_Rank, T_Rank).
can_step(Board, -1,  1, F_File-F_Rank, T_File-T_Rank) :-
	minus_one(F_File, I_File),
	plus_one(F_Rank, I_Rank),
	\+ occupied_by(Board, I_File-I_Rank, _, _),
	can_step(Board, -1,  1, I_File-I_Rank, T_File-T_Rank).
can_step(Board, -1,  0, F_File-Rank, T_File-Rank) :-
	minus_one(F_File, T_File).
can_step(Board, -1,  0, F_File-Rank, T_File-Rank) :-
	minus_one(F_File, I_File),
	\+ occupied_by(Board, I_File-I_Rank, _, _),
	can_step(Board, -1,  0, I_File-Rank, T_File-Rank).
can_step(Board, -1, -1, F_File-F_Rank, T_File-T_Rank) :-
	minus_one(F_File, T_File),
	minus_one(F_Rank, T_Rank).
can_step(Board, -1, -1, F_File-F_Rank, T_File-T_Rank) :-
	minus_one(F_File, I_File),
	minus_one(F_Rank, I_Rank),
	\+ occupied_by(Board, I_File-I_Rank, _, _),
	can_step(Board, -1, -1, I_File-I_Rank, T_File-T_Rank).

occupied_by(Board, File-Rank, Color, Piece) :-
	member(piece(File-Rank, Color, Piece), Board).

plus_one(1,2).	
plus_one(2,3).	
plus_one(3,4).	
plus_one(4,5).	
plus_one(5,6).	
plus_one(6,7).	
plus_one(7,8).	

plus_one(a,b).
plus_one(b,c).
plus_one(c,d).
plus_one(d,e).
plus_one(e,f).
plus_one(f,g).
plus_one(g,h).

minus_one(X,Y) :-
	plus_one(Y,X).

plus_two(1,3).	
plus_two(2,4).	
plus_two(3,5).	
plus_two(4,6).	
plus_two(5,7).	
plus_two(6,8).	

plus_two(a,c).
plus_two(b,d).
plus_two(c,e).
plus_two(d,f).
plus_two(e,g).
plus_two(f,h).

minus_two(X,Y) :-
	plus_two(Y,X).


% ------------------------------------------------------------------------------
% Book moves

book( [ state(white, WhiteKing, WhiteKingRook, WhiteQueenRook), % e2e4
	state(black, BlackKing, BlackKingRook, BlackQueenRook), % respond with
	piece(a-8, black, rook  ), piece(b-8, black, night ), 	% ...   e7e5
	piece(c-8, black, bishop), piece(d-8, black, queen ),
	piece(e-8, black, king  ), piece(f-8, black, bishop),
	piece(g-8, black, night ), piece(h-8, black, rook  ),
	piece(a-7, black, pawn  ), piece(b-7, black, pawn  ),
	piece(c-7, black, pawn  ), piece(d-7, black, pawn  ),
	piece(e-7, black, pawn  ), piece(f-7, black, pawn  ),
	piece(g-7, black, pawn  ), piece(h-7, black, pawn  ),
	piece(a-1, white, rook  ), piece(b-1, white, night ),
	piece(c-1, white, bishop), piece(d-1, white, queen ),
	piece(e-1, white, king  ), piece(f-1, white, bishop),
	piece(g-1, white, night ), piece(h-1, white, rook  ),
	piece(a-2, white, pawn  ), piece(b-2, white, pawn  ),
	piece(c-2, white, pawn  ), piece(d-2, white, pawn  ), 
	piece(f-2, white, pawn  ), piece(g-2, white, pawn  ),
	piece(h-2, white, pawn  ), piece(e-4, white, pawn  ) ], e-7, e-5).
book( [ state(white, WhiteKing, WhiteKingRook, WhiteQueenRook), % e2e4  e7e5
	state(black, BlackKing, BlackKingRook, BlackQueenRook), % g1f3
	piece(a-8, black, rook  ), piece(b-8, black, night ), 	% respond with	
	piece(c-8, black, bishop), piece(d-8, black, queen ),	% ...   b8c6
	piece(e-8, black, king  ), piece(f-8, black, bishop),
	piece(g-8, black, night ), piece(h-8, black, rook  ),
	piece(a-7, black, pawn  ), piece(b-7, black, pawn  ),
	piece(c-7, black, pawn  ), piece(d-7, black, pawn  ),
	piece(f-7, black, pawn  ), piece(g-7, black, pawn  ), 
	piece(h-7, black, pawn  ), piece(a-1, white, rook  ), 
	piece(b-1, white, night ), piece(c-1, white, bishop), 
	piece(d-1, white, queen ), piece(e-1, white, king  ), 
	piece(f-1, white, bishop), piece(h-1, white, rook  ),
	piece(a-2, white, pawn  ), piece(b-2, white, pawn  ),
	piece(c-2, white, pawn  ), piece(d-2, white, pawn  ), 
	piece(f-2, white, pawn  ), piece(g-2, white, pawn  ),
	piece(h-2, white, pawn  ), piece(e-4, white, pawn  ),
	piece(e-5, black, pawn  ), piece(f-3, white, night ) ], b-8, c-6).
book( [ state(white, WhiteKing, WhiteKingRook, WhiteQueenRook), % e2e4  e7e5
	state(black, BlackKing, BlackKingRook, BlackQueenRook), % g1f3  b8f6
	piece(a-8, black, rook  ), piece(c-8, black, bishop),	% f1b5
	piece(d-8, black, queen ), piece(e-8, black, king  ), 	% respond with	
	piece(f-8, black, bishop), piece(g-8, black, night ), 	% ...   a7a6
	piece(h-8, black, rook  ), piece(a-7, black, pawn  ), 
	piece(b-7, black, pawn  ), piece(c-7, black, pawn  ), 
	piece(d-7, black, pawn  ), piece(f-7, black, pawn  ), 
	piece(g-7, black, pawn  ), piece(h-7, black, pawn  ), 
	piece(a-1, white, rook  ), piece(b-1, white, night ), 
	piece(c-1, white, bishop), piece(d-1, white, queen ), 
	piece(e-1, white, king  ), piece(h-1, white, rook  ),
	piece(a-2, white, pawn  ), piece(b-2, white, pawn  ),
	piece(c-2, white, pawn  ), piece(d-2, white, pawn  ), 
	piece(f-2, white, pawn  ), piece(g-2, white, pawn  ),
	piece(h-2, white, pawn  ), piece(e-4, white, pawn  ),
	piece(e-5, black, pawn  ), piece(f-3, white, night ),
	piece(c-6, black, night ), piece(b-5, white, bishop) ], a-7, a-6).

% ------------------------------------------------------------------------------
% Utilities

opposite(white, black).
opposite(black, white).

