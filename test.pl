%%%%%%%%%%%%%%%%%%%
%%% Test module %%%
%%%%%%%%%%%%%%%%%%%

:- [testBoard].

:- begin_tests(toast).

test(grid, [fail]) :-
	grid(0, 0, X).

test(grid, [fail]) :-
	grid(0, 1, X).

test(grid, [fail]) :-
	grid(1, 0, X).

test(grid, [fail]) :-
	grid(9, 1, X).

test(grid, [fail]) :-
	grid(1, 9, X).

test(grid, [fail]) :-
	grid(9, 9, X).

test(grid, [fail]) :-
	grid(1, 1, a).

test(grid, [all(X == [b, w, 0])]) :-
	grid(1, 1, X).

test(reverse) :-
    reverse_piece(w, b).

test(reverse) :-
	reverse_piece(b, w).

test(reverse, [all(X == [b, w])]) :-
	reverse_piece(X, Y).

test(reverse, [all(Y == [w, b])]) :-
	reverse_piece(X, Y).

test(remove) :-
	remove(3, [1, 2, 3, 4, 5], [1, 2, 4, 5]).

test(direction, [fail]) :-
	direction(0, _, _);
	direction(9, _, _).

test(color) :- 
	color(b, grid(1, 1, b)),
	color(w, grid(1, 1, w)),
	color(0, grid(1, 1, 0)).

test(color, [fail]) :-
	color(b, grid(1, 1, w)).

test(count) :-
	init_board,
	count(b, 2),
	count(w, 2).

test(validate_board) :-
	init_board,
	validate_move(4, 6, b),
	validate_move(6, 4, b),
	validate_move(3, 5, b),
	validate_move(5, 3, b).

test(validate_board) :-
	init_board,
	validate_move(3, 4, w),
	validate_move(4, 3, w),
	validate_move(5, 6, w),
	validate_move(6, 5, w).

test(validate_board, [fail]) :-
	init_board,
	validate_move(1, 1, _).

test(validate_board, [fail]) :-
	init_board,
	validate_move(4, 4, _);
	validate_move(4, 5, _);
	validate_move(5, 4, _);
	validate_move(5, 5, _).

:- end_tests(toast).