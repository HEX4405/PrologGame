:- dynamic board/1.  %list of coordinates and stats of each grid in the board
:- dynamic buffer/2.  %list of turnable pieces

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition and init

%grid(X,Y,grid) : 
	% X,Y coordinates
	% Color : 0 for none, b for black, w for white
grid(X,Y,Color) :- 
	X < 9,X > 0,Y < 9,Y > 0,
	(Color = b ; Color = w ; Color = 0).


%initialisation of the borad
init_board() :-
assert(board([
grid(1,1,0),grid(1,2,0),grid(1,3,0),grid(1,4,0),grid(1,5,0),grid(1,6,0),grid(1,7,0),grid(1,8,0),
grid(2,1,0),grid(2,2,0),grid(2,3,0),grid(2,4,0),grid(2,5,0),grid(2,6,0),grid(2,7,0),grid(2,8,0),
grid(3,1,0),grid(3,2,0),grid(3,3,0),grid(3,4,0),grid(3,5,0),grid(3,6,0),grid(3,7,0),grid(3,8,0),
grid(4,1,0),grid(4,2,0),grid(4,3,0),grid(4,4,b),grid(4,5,w),grid(4,6,0),grid(4,7,0),grid(4,8,0),
grid(5,1,0),grid(5,2,0),grid(5,3,0),grid(5,4,w),grid(5,5,b),grid(5,6,0),grid(5,7,0),grid(5,8,0),
grid(6,1,0),grid(6,2,0),grid(6,3,0),grid(6,4,0),grid(6,5,0),grid(6,6,0),grid(6,7,0),grid(6,8,0),
grid(7,1,0),grid(7,2,0),grid(7,3,0),grid(7,4,0),grid(7,5,0),grid(7,6,0),grid(7,7,0),grid(7,8,0),
grid(8,1,0),grid(8,2,0),grid(8,3,0),grid(8,4,0),grid(8,5,0),grid(8,6,0),grid(8,7,0),grid(8,8,0)])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utils

%check if a grid is actualy in the board
find_grid(Grid, [Grid|_]).
find_grid(Grid, [_|Rest]) :-
	find_grid(Grid, Rest).

%change color of a grid
reverse_piece(b, w).
reverse_piece(w, b).
	
%remove an element from a list
%WARNING : do not use it alone!!!
remove(X, [X|Y], Y).
remove(X, [X1|Y], [X1|Z]) :- remove(X,Y,Z).
	
%place or change a piece without checking feasability
%WARNING : do not use it alone!!!
place(Row,Col,Color,Board,NewBoard) :-
	board(Board),
	remove(grid(Row,Col,_),Board,Board1),
	NewBoard = [grid(Row,Col,Color)|Board1],
	retract(board(_)),
	assert(board(NewBoard)).
	
%mark eight directions
direction(Index, Row, Col) :-
	(Index = 1, Row is -1, Col is 0);
	(Index = 2, Row is -1, Col is 1);
	(Index = 3, Row is 0, Col is 1);
	(Index = 4, Row is 1, Col is 1);
	(Index = 5, Row is 1, Col is 0);
	(Index = 6, Row is 1, Col is -1);
	(Index = 7, Row is 0, Col is -1);
	(Index = 8, Row is -1, Col is -1).
	
%TODO count pieces for a player
count(Color,Board,Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Validation and flip

%check feasability of a move 
validate_move(Row,Col,Color) :-
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
	buffer(_,_),    %a move is not feasable if there's no turnable pieces after move
	listing(buffer(_,_)).   %remove this line when game done.

%check feasability in one direction
check_direction(Row,Col,Color,Board,Direction) :-
	direction(Direction, R, C),
	Row1 is Row+R, Col1 is Col+C,
	reverse_piece(Color,Color1),
	check_direction1(Row1,Col1,Color1,Board,Direction).
check_direction1(Row,Col,Color,Board,Direction) :-
	find_grid(grid(Row,Col,Color),Board),
	direction(Direction, R, C),
	Row1 is Row+R, Col1 is Col+C,
	reverse_piece(Color,Color1),
	(find_grid(grid(Row1,Col1,Color1),Board);
	check_direction1(Row1,Col1,Color,Board,Direction)),
	assert(buffer(Row,Col)).

%check feasability, place piece, turn pieces
make_move(Row,Col,Color) :-
	retractall(buffer(_,_)),
	validate_move(Row,Col,Color),
	board(Board),
	place(Row,Col,Color,Board,NewBoard),
	flip_buffer(Color,NewBoard),
	print_board(_),
	!.

%turn pieces
flip_buffer(_,_) :- 
	not(buffer(_,_)),
	!.
flip_buffer(Color,Board) :-
	buffer(Row,Col),
	place(Row,Col,Color,Board,NewBoard),
	retract(buffer(Row,Col)),
	flip_buffer(Color,NewBoard).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Printing

%print the board	
print_board(Board) :- 
	board(Board),
	nl, write('+--+--+--+--+--+--+--+--+--+'), 
	nl, write('|  | 1| 2| 3| 4| 5| 6| 7| 8|'), 
	nl, write('+--+--+--+--+--+--+--+--+--+'), 
	nl, print_row(1, Board), 
	!.

%end condition of a row
print_row(9,_) :- true.

%print a row
print_row(Row, Board) :-
  Row < 9,
  write('| '), write(Row),
  print_col(Row, 1, Board),
  Row1 is Row + 1,
  print_row(Row1, Board),
  !.

 %end condition of a column
 print_col(_,9,_) :- 
	write('|'), nl, write('+--+--+--+--+--+--+--+--+--+'), nl.
 
 %print a column
print_col(Row, Col, Board) :-
  find_grid(grid(Row, Col, Color), Board),
  write('|'), write(' '), print_color(Color),
  Col1 is Col + 1,
  print_col(Row, Col1, Board).

%print @ for black pieces, O for white pieces
 print_color(Color) :-
	(Color = b,write('@'));(Color = w,write('O'));(Color = 0,write(' ')).
