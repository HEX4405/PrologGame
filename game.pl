%%%%%%%%%%%%%%%%%%%
%%% Game module %%%
%%%%%%%%%%%%%%%%%%%

:- dynamic buffer/2. 

% turn pieces
flip_buffer(_,_) :- 
  not(buffer(_,_)),
  !.
  
flip_buffer(Color, Board) :-
  buffer(Row, Col),
  place(Row, Col, Color, Board, NewBoard),
  retract(buffer(Row, Col)),
  flip_buffer(Color, NewBoard).

drawPos() :-
    count(0,NbVide),
    NbVide = 0,
    count(w,Result),
    count(b,Result2),
    Result = Result2.

winPos(J1) :-
  count(0,NbVide),
    NbVide = 0,
  count(J1,Result),
  nextPlayer(J1, J2),
    count(J2,Result2),
    Result > Result2.

% check feasability, place piece, turn pieces
make_move(Row, Col, Color) :-
  retractall(buffer(_,_)),
  validate_move(Row,Col,Color),
  board(Board),
  place(Row,Col,Color,Board,NewBoard),
  flip_buffer(Color,NewBoard),
  !.

make_move([X1, play, Board], [X2,State], Pos1, Pos2) :-
    make_move(Pos1, Pos2, X1),
    (
      winPos(X1), !, State = win ;
      winPos(X2), !, State = win2 ;
      drawPos(), !, State = draw ;
      State = play
    ).