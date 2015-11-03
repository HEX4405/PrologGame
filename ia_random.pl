%%%%%%%%%%%%%%%%%%%%%%%%
%%% IA random module %%%
%%%%%%%%%%%%%%%%%%%%%%%%

random_move(Player, Pos1, Pos2, Compteur) :- 
		  random_between(1, 8, Pos1),
		  random_between(1, 8, Pos2),
		  validate_move(Pos1, Pos2, Player);
		  NextCompteur is Compteur +1,
		  NextCompteur < 10000,  
		  random_move(Player, Pos1, Pos2, NextCompteur).

random_move(Player, Pos1, Pos2) :- random_move(Player, Pos1, Pos2, 0).