%%%%%%%%%%%%%%%%%%%%%%%%
%%% IA minmax module %%%
%%%%%%%%%%%%%%%%%%%%%%%%

minimax(Player,Pos, BestNextPos, Val):-
						% Pos has successors
	bagof(NextPos, move(Pos, NextPos), NextPosList),

    best(Player,NextPosList, BestNextPos, Val), !.

minimax(Player,Pos, _, Val) :-                     			% Pos has no successors
    utility(Player,Pos, Val).


best(Player,[Pos], Pos, Val) :- 
	                               % There is no more position to compare
    minimax(Player, Pos, _, Val), !.

best(Player,[Pos1 | PosList], BestPos, BestVal) :-  
	           % There are other positions to compare
   minimax(Player,Pos1, _, Val1),
    best(Player,PosList, Pos2, Val2),

    betterOf(Player,Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterOf(Player,Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    min_to_move(Pos0),
                             % MIN to move in Pos0
    Val0 > Val1, !.                            % MAX prefers the greater value

betterOf(Player,Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    max_to_move(Pos0),                         % MAX to move in Pos0
    Val0 < Val1, !.                            % MIN prefers the lesser value

betterOf(Player,_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0

bestMove(Player, [Player, play,Board], [NextPlayer, State, NextBoard]) :-
	nextPlayer(Player,NextPlayer),
  minimax(Player,[Player, play,Board], [NextPlayer, State, NextBoard], _),
  retract(board(_)),
	assert(board(NextBoard)).


place_no_modification(Row,Col,Color,Board,NewBoard) :-
	remove(grid(Row,Col,_),Board,Board1),
	NewBoard = [grid(Row,Col,Color)|Board1].

computer_move(Color, Board, NextBoard) :-
	validate_move(Board,Row,Col,Color)->
	place_no_modification(Row,Col,Color,Board,Board1),
	flip_buffer(Color,Board1,NextBoard),
	!
	;
	NextBoard=Board,
	!.

move([X1, play, Board], [X2, win, NextBoard]) :-
    nextPlayer(X1, X2),
    computer_move(X1, Board, NextBoard),
    winPos(X1,NextBoard), !.

move([X1, play, Board], [X2, draw, NextBoard]) :-
    nextPlayer(X1, X2),
    computer_move(X1, Board, NextBoard),
    drawPos(X1,NextBoard), !.

move([X1, play, Board], [X2, play, NextBoard]) :-
    nextPlayer(X1, X2),
    computer_move(X1, Board, NextBoard).

min_to_move([w, _, _]).
max_to_move([b, _, _]).

utility(Player, [Player, win, _], 1).     % Previous player (MAX) has win.
utility(Player, [NextPlayer, win, _], -1):- nextPlayer(Player, NextPlayer).      % Previous player (MIN) has win.
utility(_, [_, draw, _], 0).

%% playMinMax :-
%% 	  retract(board(_))->
%% 	  init_board,
%% 	  board(Board),
%% 	  playMinMax([w, play, Board], w)
%% 	  ;
%% 	  init_board,
%% 	  board(Board),
%% 	  playMinMax([w, play, Board], w).

%% % play(+Position, +HumanPlayer)
%% % If next player to play in position is equal to HumanPlayer -> Human must play
%% % Ask to human what to do.
%% playMinMax([Player, play, Board], Player) :- !,
%%      (
%%       bestMove(Player, [Player, play, Board], [NextPlayer, State, NextBoard]), !,
%%       print_board(NextBoard),
%%       (
%%         State = win, !,                             % If Player win -> stop
%%         nl, write('End of game : '),
%%         write(Player), write(' win !'), nl, nl
%%         ;
%%         State = draw, !,                            % If draw -> stop
%%         nl, write('End of game : '),
%%         write(' draw !'), nl, nl
%%         ;
%%         playMinMax([NextPlayer, play, NextBoard], NextPlayer) % Else -> continue the game
%%       )
%%       ;
%%       playMinMax([NextPlayer, play, NextBoard], NextPlayer)        %If player can't play -> NextPlayer
%%     ).