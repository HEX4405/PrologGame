%%%%%%%%%%%%%%%%%%%%
%%% Model module %%%
%%%%%%%%%%%%%%%%%%%%

:- dynamic board/1.
:- dynamic evaluation_board/1. 

% grid(X, Y, grid) : 
	% X, Y coordinates
	% Color : 0 for none, b for black, w for white
grid(X, Y, Color) :- 
	X < 9, X > 0, 
	Y < 9, Y > 0,
	(Color = b ; Color = w ; Color = 0).

% evalgrid(X, Y, grid) : 
	% X,Y coordinates
evalgrid(X, Y, _):-
	X < 9, X > 0,
	Y < 9,Y > 0.

%initialisation of the board
init_board :-
retractall(board(_)),
assert( board([ grid(1,1,0),grid(1,2,0),grid(1,3,0),grid(1,4,0),grid(1,5,0),grid(1,6,0),grid(1,7,0),grid(1,8,0),
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

% change color of a grid
reverse_piece(b, w).
reverse_piece(w, b).

% indicate the next player
% only a semantic difference with reverse_piece
nextPlayer(b, w).
nextPlayer(w, b).
