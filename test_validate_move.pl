:- dynamic buffer/2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Validation and flip

reverse_piece(b, w).
reverse_piece(w, b).

direction(Index, Row, Col) :-
(Index = 1, Row is -1, Col is 0);
(Index = 2, Row is -1, Col is 1);
(Index = 3, Row is 0, Col is 1);
(Index = 4, Row is 1, Col is 1);
(Index = 5, Row is 1, Col is 0);
(Index = 6, Row is 1, Col is -1);
(Index = 7, Row is 0, Col is -1);
(Index = 8, Row is -1, Col is -1).

validate_move(Row,Col,Color) :-
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
listing(buffer(X,Y)).

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

make_move(Row,Col,Color) :-
retractall(buffer(_,_)),
validate_move(Row,Col,Color),
board(Board),
place(Row,Col,Color,Board,NewBoard),
flip_buffer(Color,NewBoard),
print_board(_),!.

flip_buffer(_,_) :- not(buffer(_,_)),!.
flip_buffer(Color,Board) :-
buffer(Row,Col),
place(Row,Col,Color,Board,NewBoard),
retract(buffer(Row,Col)),
flip_buffer(Color,NewBoard).