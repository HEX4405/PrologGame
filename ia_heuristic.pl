%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IA heuristic module %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic valid_move/3.
:- dynamic buffer/2.  

find_valid_move1(Player, []).
		
find_valid_move1(Player,[grid(X, Y, _)| Rest]):-
	validate_move(X, Y, Player),
	not(valid_move(X, Y, Player)),
	assert(valid_move(X, Y, Player)),
	find_valid_move1(Player, Rest),
	!.
	
find_valid_move(Player, Board):-
	retractall(valid_move(_)),
	find_valid_move1(Player, Board).
	

heuristic_move(Player, Pos1, Pos2, NewResult) :- 
	validate_move(Pos1, Pos2, Player),
	make_fake_move(Player, Pos1, Pos2, NewBoard),
	evaluate_heuristic(NewBoard, Player, Result),
	max(NewResult, Result, BestResult),
	heuristic_move(Player, NPos1 , NPos2, BestResult).
	
	
best_result(Player, Pos1, Pos2, Result) :- 
	validate_move(Pos1, Pos2, Player),
	make_fake_move(Player, Pos1, Pos2, NewBoard),
	evaluate_heuristic(NewBoard, Player, Result),
	max(NewResult, Result, BestResult).
	

is_winner(Board, Color) :-
	count(0, Board, NbVide),
	NbVide =:= 0,
	count(Color, Board, Result),
	reverse_piece(Color, Color2),
	count(Color2, Board, Result2),
	Result > Result2.
	
is_draw(Board, Color) :-
	count(0, Board, NbVide),
	NbVide =:= 0,
	count(Color, Board, Result),
	reverse_piece(Color, Color2),
	count(Color2, Board, Result2),
	Result =:= Result2.

heuristic([
value(1,1,500),	value(1,2,-150),	value(1,3,30),	value(1,4,10),	value(1,5,10),	value(1,6,30),	value(1,7,-150),	value(1,8,500),
value(2,1,-150),	value(2,2,-250),	value(2,3,0),	value(2,4,0),	value(2,5,0),	value(2,6,0),	value(2,7,-250),	value(2,8,-150),
value(3,1,30),	value(3,2,0),	value(3,3,1),	value(3,4,2),	value(3,5,2),	value(3,6,1),	value(3,7,0),	value(3,8,30),
value(4,1,10),	value(4,2,0),	value(4,3,2),	value(4,4,16),	value(4,5,16),	value(4,6,2),	value(4,7,0),	value(4,8,10),
value(5,1,10),	value(5,2,0),	value(5,3,2),	value(5,4,16),	value(5,5,16),	value(5,6,2),	value(5,7,0),	value(5,8,10),
value(6,1,30),	value(6,2,0),	value(6,3,1),	value(6,4,2),	value(6,5,2),	value(6,6,1),	value(6,7,0),	value(6,8,30),
value(7,1,-150),	value(7,2,-250),	value(7,3,0),	value(7,4,0),	value(7,5,0),	value(7,6,0),	value(7,7,-150),	value(7,8,-150),
value(8,1,500),	value(8,2,-150),	value(8,3,30),	value(8,4,10),	value(8,5,10),	value(8,6,30),	value(8,7,-250),	value(8,8,500)]).

evaluate_heuristic(Board, Color, Result) :-
	(is_winner(Board, Color), Result is 2500, !);
	(reverse_piece(Color, Color2), is_winner(Board, Color2), Result is -2500, !);
	(is_draw(Board, Color), Result is 0, !);
	evaluate_heuristic2(Board, Color, Result).

evaluate_heuristic2([], _, 0).	
	
evaluate_heuristic2([grid(X, Y, Piece)| Board], Color, Result) :-
	(Piece = Color,
	heuristic(H), find_grid(value(X, Y, Value), H),
	evaluate_heuristic2(Board, Color, Result2),
	Result is Result2 + Value);
	(not(Piece = Color),
	evaluate_heuristic2(Board, Color, Result)).
	
make_fake_move(Row, Col, Color, Board, NewBoard1) :-
	retractall(buffer(_, _)),
	validate_move(Row, Col, Color),
	fake_place(Row, Col, Color, Board, NewBoard),
	fake_flip_buffer(Color, NewBoard, NewBoard1),
	!.
	
fake_place(Row, Col, Color, Board, NewBoard) :-
	remove(grid(Row, Col,_), Board, Board1),
	NewBoard = [grid(Row, Col, Color)| Board1].
	
fake_flip_buffer(_, _, _):-
	not(buffer(_, _)),
	!.
fake_flip_buffer(Color, Board, NewBoard):-
	buffer(Row, Col),
	fake_place(Row, Col, Color, Board, NewBoard),
	retract(buffer(Row, Col)),
	fake_flip_buffer(Color, NewBoard, NewBoard1).