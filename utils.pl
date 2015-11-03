%%%%%%%%%%%%%%%%%%%%
%%% Utils module %%%
%%%%%%%%%%%%%%%%%%%%

:- dynamic board/1. 
:- dynamic buffer/2. 

% check if a grid is actualy in the board
find_grid(Grid, [Grid|_]).
find_grid(Grid, [_|Rest]) :-
	find_grid(Grid, Rest).
	
% remove an element from a list
% WARNING : do not use it alone
remove(X, [X|Y], Y).
remove(X, [X1|Y], [X1|Z]) :- remove(X,Y,Z).
	
% place or change a piece without checking feasability
% WARNING : do not use it alone
place(Row, Col, Color, Board, NewBoard) :-
	board(Board),
	remove(grid(Row, Col, _), Board, Board1),
	NewBoard = [grid(Row, Col, Color)| Board1],
	retract(board(_)),
	assert(board(NewBoard)).
	
% mark eight directions
direction(Index, Row, Col) :-
	(Index = 1, Row is -1, Col is 0);
	(Index = 2, Row is -1, Col is 1);
	(Index = 3, Row is 0, Col is 1);
	(Index = 4, Row is 1, Col is 1);
	(Index = 5, Row is 1, Col is 0);
	(Index = 6, Row is 1, Col is -1);
	(Index = 7, Row is 0, Col is -1);
	(Index = 8, Row is -1, Col is -1).

% check stat of a grid
color(Color, grid(_, _, C)):-
	Color = C.
	
% count pieces for a player
count(Color, [], 0):- !.
count(Color, [X|T], N) :- 
	color(Color, X),
    count(Color, T, N1), 
    N is N1 + 1.

count(Color, [Y|T], N) :- 
    not(color(Color,Y)),
    count(Color, T, N).

count(Color, Result):-
	board(Board),
	count(Color, Board, Result).

% find max between X and Y
max(X,Y,Y) :- X =< Y, !.
max(X,Y,X). 

% check feasability of a move 
validate_move(Row, Col, Color) :-
	retractall(buffer(_,_)),   %empty the buffer
	board(Board),
	find_grid(grid(Row,Col,0),Board), %check if the grid is empty
	((check_direction(Row,Col,Color,Board,1);true),
	(check_direction(Row,Col,Color,Board,2);true),
	(check_direction(Row,Col,Color,Board,3);true),
	(check_direction(Row,Col,Color,Board,4);true),
	(check_direction(Row,Col,Color,Board,5);true),
	(check_direction(Row,Col,Color,Board,6);true),
	(check_direction(Row,Col,Color,Board,7);true),
	(check_direction(Row,Col,Color,Board,8);true)),
	buffer(_,_).   %a move is not feasable if there's no turnable pieces after move

validate_move(Board, Row, Col, Color) :-
	retractall(buffer(_,_)),   %empty the buffer
	find_grid(grid(Row,Col,0),Board), %check if the grid is empty
	((check_direction(Row,Col,Color,Board,1);true),
	(check_direction(Row,Col,Color,Board,2);true),
	(check_direction(Row,Col,Color,Board,3);true),
	(check_direction(Row,Col,Color,Board,4);true),
	(check_direction(Row,Col,Color,Board,5);true),
	(check_direction(Row,Col,Color,Board,6);true),
	(check_direction(Row,Col,Color,Board,7);true),
	(check_direction(Row,Col,Color,Board,8);true)),
	buffer(_,_).  

% check feasability in one direction
check_direction(Row,Col,Color,Board,Direction) :-
	direction(Direction, R, C),
	Row1 is Row+R, Col1 is Col+C,
	reverse_piece(Color,Color1),
	check_direction1(Row1,Col1,Color1,Board,Direction).

check_direction1(Row,Col,Color,Board,Direction) :-
	find_grid(grid(Row,Col,Color),Board),
	direction(Direction, R, C),
	Row1 is Row+R, Col1 is Col+C,
	reverse_piece(Color, Color1),
	(find_grid(grid(Row1, Col1, Color1), Board);
	check_direction1(Row1, Col1, Color, Board, Direction)),
	assert(buffer(Row, Col)).

