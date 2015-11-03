%%%%%%%%%%%%%%%%%%%%
%%% Print module %%%
%%%%%%%%%%%%%%%%%%%%

%print the board	
print_board :- 
	board(Board),
	nl, write('+--+--+--+--+--+--+--+--+--+'), 
	nl, write('|  | 1| 2| 3| 4| 5| 6| 7| 8|'), 
	nl, write('+--+--+--+--+--+--+--+--+--+'), 
	nl, print_row(1, Board), 
	!.

print_board(Board):- 
  nl, write('+--+--+--+--+--+--+--+--+--+'), 
  nl, write('|  | 1| 2| 3| 4| 5| 6| 7| 8|'), 
  nl, write('+--+--+--+--+--+--+--+--+--+'), 
  nl, print_row(1, Board), 
  !.

% end condition of a row
print_row(9,_) :- true.

% print a row
print_row(Row, Board) :-
  Row < 9,
  write('| '), write(Row),
  print_col(Row, 1, Board),
  Row1 is Row + 1,
  print_row(Row1, Board),
  !.

 % end condition of a column
 print_col(_,9,_) :- 
	write('|'), nl, write('+--+--+--+--+--+--+--+--+--+'), nl.
 
 % print a column
print_col(Row, Col, Board) :-
  find_grid(grid(Row, Col, Color), Board),
  write('|'), write(' '), print_color(Color),
  Col1 is Col + 1,
  print_col(Row, Col1, Board).

% print @ for black pieces, O for white pieces
 print_color(Color) :-
	(Color = b,write('@'));(Color = w,write('O'));(Color = 0,write(' ')).