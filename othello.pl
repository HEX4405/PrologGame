%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main : Othello module %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [utils].
:- [print].
:- [model].
:- [ia_random].

:- dynamic buffer/2. 

% turn pieces
flip_buffer(_, _) :- 
  not(buffer(_, _)),
  !.
  
flip_buffer(Color, Board) :-
  buffer(Row, Col),
  place(Row, Col, Color, Board, NewBoard),
  retract(buffer(Row, Col)),
  flip_buffer(Color, NewBoard).

drawPos() :-
    count(0, NbVide),
    NbVide = 0,
    count(w, Result),
    count(b, Result2),
    Result = Result2.

winPos(J1) :-
  count(0, NbVide),
    NbVide = 0,
  count(J1, Result),
  nextPlayer(J1, J2),
    count(J2, Result2),
    Result > Result2.

% check feasability, place piece, turn pieces
make_move(Row, Col, Color) :-
  retractall(buffer(_,_)),
  validate_move(Row, Col, Color),
  board(Board),
  place(Row, Col, Color, Board, NewBoard),
  flip_buffer(Color, NewBoard),
  !.

make_move([X1, play, Board], [X2, State], Pos1, Pos2) :-
    make_move(Pos1, Pos2, X1),
    (
      winPos(X1), !, State = win ;
      winPos(X2), !, State = win2 ;
      drawPos(), !, State = draw ;
      State = play
    ).


% play : Predicate that launches the game
play :-
	  init_board,
	  print_board,
	  play([w, play, Board], Player).


% play(+Position, +Player)
play([Player, play, Board], Player) :- !,
    (
  	  random_move(Player, Pos1, Pos2, 0),
  	  nextPlayer(Player, NextPlayer),
      make_move([Player, play, Board], [NextPlayer, State], Pos1, Pos2), !,
  	  
      print_board(),
      (
        State = win, !,                             % If Player win -> stop
        nl, write('End of game : '),
        write(Player), write(' win !'), nl, nl
        ;
        State = win2, !,                             % If Player win -> stop
        nl, write('End of game : '),
        write(NextPlayer), write(' win !'), nl, nl
        ;
        State = draw, !,                            % If draw -> stop
        nl, write('End of game : '),
        write(' draw !'), nl, nl
        ;
        play([NextPlayer, play, Board], NextPlayer) % Else -> continue the game
      )
      ;
      play([NextPlayer, play, Board], NextPlayer)        % If can't play, let next player play
    ).