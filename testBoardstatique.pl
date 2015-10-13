%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition and init

%grid(X,Y,grid) : 
	% X,Y coordinates
	% Color : 0 for none, b for black, w for white
grid(X,Y,Color) :- 
	X < 9,X > 0,Y < 9,Y > 0,
	(Color = b ; Color = w ; Color = 0).


%initialisation of the borad
board([
grid(1,1,0),grid(1,2,0),grid(1,3,0),grid(1,4,0),grid(1,5,0),grid(1,6,0),grid(1,7,0),grid(1,8,0),
grid(2,1,0),grid(2,2,0),grid(2,3,0),grid(2,4,0),grid(2,5,0),grid(2,6,0),grid(2,7,0),grid(2,8,0),
grid(3,1,0),grid(3,2,0),grid(3,3,0),grid(3,4,0),grid(3,5,0),grid(3,6,0),grid(3,7,0),grid(3,8,0),
grid(4,1,0),grid(4,2,0),grid(4,3,0),grid(4,4,b),grid(4,5,w),grid(4,6,0),grid(4,7,0),grid(4,8,0),
grid(5,1,0),grid(5,2,0),grid(5,3,0),grid(5,4,w),grid(5,5,b),grid(5,6,0),grid(5,7,0),grid(5,8,0),
grid(6,1,0),grid(6,2,0),grid(6,3,0),grid(6,4,0),grid(6,5,0),grid(6,6,0),grid(6,7,0),grid(6,8,0),
grid(7,1,0),grid(7,2,0),grid(7,3,0),grid(7,4,0),grid(7,5,0),grid(7,6,0),grid(7,7,0),grid(7,8,0),
grid(8,1,0),grid(8,2,0),grid(8,3,0),grid(8,4,0),grid(8,5,0),grid(8,6,0),grid(8,7,0),grid(8,8,0)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utils

%check if a grid is actualy in the board
find_grid(Grid, [Grid|_]).
find_grid(Grid, [_|Rest]) :-
	find_grid(Grid, Rest).
	
%remove an element from a list
%WARNING : do not use it alone!!!
remove(X, [X|Y], Y).
remove(X, [X1|Y], [X1|Z]) :- remove(X,Y,Z).
	
%place or change a piece without checking feasability
%WARNING : do not use it alone!!!
place(Row,Col,Color,Board,NewBoard) :-
	remove(grid(Row,Col,_),Board,Board1),
	NewBoard = [grid(Row,Col,Color)|Board1].
	
%place a piece and flip any other pieces may concern
move(Row,Col,Color,Board,NewBoard) :-
	valide_move(Row,Col,Color,Board),
	turn_pieces(Row,Col,Color,Board,NewBoard).
	
%check if the move is feasible
valide_move(Row,Col,Color,Board) :-
	find_grid(grid(Row,Col,0),Board), %check if the grid is empty
	find_opponent(Row,Col,Color,Board). 
	
%TODO :  check if in a direction there is at least an opposite piece
find_opponent(_,_,_,Board) :- true. %TestMark
%fo_up_left(), fo_up(), ... fo_down_right() for eight directions.

%TODO :  flip pieces after moves
turn_pieces(Row,Col,Color,Board,NewBoard) :- true.
%tp_up_left(), tp_up(), ... tp_down_right() for eight directions.

%TODO count pieces for a player
count(Color,Board,Result).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Printing

%print the board	
print(Board) :- 
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
