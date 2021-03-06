:- dynamic board/1.  %list of coordinates and stats of each grid in the board
:- dynamic evaluation_board/1.
:- dynamic buffer/2.  %list of turnable pieces

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition and init

%grid(X,Y,grid) : 
	% X,Y coordinates
	% Color : 0 for none, b for black, w for white
grid(X,Y,Color) :- 
	X < 9,X > 0,Y < 9,Y > 0,
	(Color = b ; Color = w ; Color = 0).

evalgrid(X,Y,_):-
	X < 9,X > 0,Y < 9,Y > 0.


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
grid(8,1,0),grid(8,2,0),grid(8,3,0),grid(8,4,0),grid(8,5,0),grid(8,6,0),grid(8,7,0),grid(8,8,0)])),

assert(evaluation_board([
evalgrid(1,1,0),evalgrid(1,2,0),evalgrid(1,3,0),evalgrid(1,4,0),evalgrid(1,5,0),evalgrid(1,6,0),evalgrid(1,7,0),evalgrid(1,8,0),
evalgrid(2,1,0),evalgrid(2,2,0),evalgrid(2,3,0),evalgrid(2,4,0),evalgrid(2,5,0),evalgrid(2,6,0),evalgrid(2,7,0),evalgrid(2,8,0),
evalgrid(3,1,0),evalgrid(3,2,0),evalgrid(3,3,0),evalgrid(3,4,0),evalgrid(3,5,0),evalgrid(3,6,0),evalgrid(3,7,0),evalgrid(3,8,0),
evalgrid(4,1,0),evalgrid(4,2,0),evalgrid(4,3,0),evalgrid(4,4,b),evalgrid(4,5,w),evalgrid(4,6,0),evalgrid(4,7,0),evalgrid(4,8,0),
evalgrid(5,1,0),evalgrid(5,2,0),evalgrid(5,3,0),evalgrid(5,4,w),evalgrid(5,5,b),evalgrid(5,6,0),evalgrid(5,7,0),evalgrid(5,8,0),
evalgrid(6,1,0),evalgrid(6,2,0),evalgrid(6,3,0),evalgrid(6,4,0),evalgrid(6,5,0),evalgrid(6,6,0),evalgrid(6,7,0),evalgrid(6,8,0),
evalgrid(7,1,0),evalgrid(7,2,0),evalgrid(7,3,0),evalgrid(7,4,0),evalgrid(7,5,0),evalgrid(7,6,0),evalgrid(7,7,0),evalgrid(7,8,0),
evalgrid(8,1,0),evalgrid(8,2,0),evalgrid(8,3,0),evalgrid(8,4,0),evalgrid(8,5,0),evalgrid(8,6,0),evalgrid(8,7,0),evalgrid(8,8,0)])).


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

%check stat of a grid
color(Color,grid(_,_,C)):-
	Color = C.
	
%count pieces for a player
count(Color,[],0):-
	!.
count(Color, [X|T], N) :- 
	color(Color,X),
    count(Color, T, N1), 
    N is N1 + 1. 
count(Color, [Y|T], N) :- 
    not(color(Color,Y)),
    count(Color, T, N).
count_color(Color,Result):-
	board(Board),
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

%Strong IA



minimax(Pos, BestNextPos, Val):-						% Pos has successors
	bagof(NextPos, move(Pos, NextPos), NextPosList),
    best(NextPosList, BestNextPos, Val), !.

minimax(Pos, _, Val) :-                     			% Pos has no successors
    utility(Pos, Val).


best([Pos], Pos, Val) :-                                % There is no more position to compare
    minimax(Pos, _, Val), !.

best([Pos1 | PosList], BestPos, BestVal) :-             % There are other positions to compare
   minimax(Pos1, _, Val1),
    best(PosList, Pos2, Val2),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    min_to_move(Pos0),                         % MIN to move in Pos0
    Val0 > Val1, !.                            % MAX prefers the greater value

betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    max_to_move(Pos0),                         % MAX to move in Pos0
    Val0 < Val1, !.                            % MIN prefers the lesser value

betterOf(_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0

bestMove(Pos, NextPos) :-
    minimax(Pos, NextPos, _).




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





% playAskColor
% Ask the color for the human player and start the game with it.
play :-
	  init_board,
	  nl, write('Color for human player ? (w or b)'), nl,
	  read(Player), nl,
	  (
	    Player \= w, Player \= b, !,     % If not x or o -> not a valid color
	    write('Error : not a valid color !'), nl,
	    play                     % Ask again
	    ;
	    board(Board),
	    print_board(Board), nl,
	
	    % Start the game with color and emptyBoard
	    play([w, play, Board], Player)
	  ).


% play(+Position, +HumanPlayer)
% If next player to play in position is equal to HumanPlayer -> Human must play
% Ask to human what to do.
play([Player, play, Board], Player) :- !,
    nl, write('Next move ?'), nl,
    read(Pos1), nl,
    read(Pos2), nl,                               % Ask human where to play
    (
      humanMove([Player, play, Board], [NextPlayer, State], Pos1, Pos2), !,
      print_board(_),
      (
        State = win, !,                             % If Player win -> stop
        nl, write('End of game : '),
        write(Player), write(' win !'), nl, nl
        ;
        State = draw, !,                            % If draw -> stop
        nl, write('End of game : '),
        write(' draw !'), nl, nl
        ;
        play([NextPlayer, play, NextBoard], Player) % Else -> continue the game
      )
      ;
      write('-> Bad Move !'), nl,                % If humanMove fail -> bad move
      play([Player, play, Board], Player)        % Ask again
    ).

% play(+Position, +HumanPlayer)
% If it is not human who must play -> Computer must play
% Compute the best move for computer with minimax or alpha-beta.
play([Player, play, Board], HumanPlayer) :-
    nl, write('Computer play : '), nl, nl,
    % Compute the best move
    bestMove([Player, play, Board], [NextPlayer, State, BestSuccBoard]),
    print_board(BestSuccBoard),
    (
      State = win, !,                                 % If Player win -> stop
      nl, write('End of game : '),
      write(Player), write(' win !'), nl, nl
      ;
      State = draw, !,                                % If draw -> stop
      nl, write('End of game : '), write(' draw !'), nl, nl
      ;
      % Else -> continue the game
      play([NextPlayer, play, BestSuccBoard], HumanPlayer)
    ).



% nextPlayer(X1, X2)
% True if X2 is the next player to ply after X1.
nextPlayer(w, b).
nextPlayer(b, w).


% When human play
humanMove([X1, play, Board], [X2, State], Pos1, Pos2) :-
    nextPlayer(X1, X2),
    make_move(Pos1, Pos2, X1),
    (
      winPos(X1, Board), !, State = win ;
      drawPos(X1,Board), !, State = draw ;
      State = play
    ).


move([X1, play, Board], [X2, win, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1, Board, NextBoard),
    winPos(X1, NextBoard), !.

move([X1, play, Board], [X2, draw, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1, Board, NextBoard),
    drawPos(X1,NextBoard), !.

move([X1, play, Board], [X2, play, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1, Board, NextBoard).


min_to_move([w, _, _]).

max_to_move([b, _, _]).

utility([w, win, _], 1).       % Previous player (MAX) has win.
utility([b, win, _], -1).      % Previous player (MIN) has win.
utility([_, draw, _], 0).


drawPos(_,Board) :-
    count(0,Board, NbVide),
    NbVide =:= 0,
    count(w,Board,Result),
    count(b,Board,Result2),
    Result =:= Result2.

winPos(J1,Board) :-
	count(0,Board, NbVide),
    NbVide =:= 0,
	count(J1,Board,Result),
	nextPlayer(J1, J2),
    count(J2,Board,Result2),
    Result > Result2.


move_aux(P, [0|Bs], [P|Bs]).

move_aux(P, [B|Bs], [B|B2s]) :-
    move_aux(P, Bs, B2s).