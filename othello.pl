%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main : Othello module %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [utils].
:- [print].
:- [model].
:- [game].
:- [ia_random].

% play : Predicate that launches the game
play :-
	  init_board(),
	  print_board(),
	  play([w, play, Board], Player).


% play(+Position, +Player)
play([Player, play, Board], Player) :- !,
    (
  	  random_move(Player,Pos1, Pos2,0),
  	  nextPlayer(Player,NextPlayer),
      make_move([Player, play, Board], [NextPlayer,State], Pos1, Pos2), !,
  	  
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
      play([NextPlayer, play, Board], NextPlayer)        %If can't play, let next player play
    ).