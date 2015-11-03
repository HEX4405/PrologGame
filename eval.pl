%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Evaluation module                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%% Tableau des ratios de victoire %%%%%%%%%%%%%%%%%%%%%%%%
% Vic.Ratio | Naive MinMax Heuristic
% ----------+-----------------------
% Naive     |   0      0       0
% MinMax    |   0      0       0
% Heuristic |   0      0       0
victory_rates([0,0,0,  
			   0,0,0,  
			   0,0,0]).


%%%%%%%%%%%% Liste des challengers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
challengers(['Naive', 'MinMax', 'Heuristic']).


%%%%%%%%%%%% Selection d'un challenger dans la liste %%%%%%%%%%%%%%%
challenger(N, Challenger ) :- vector_access(N, challengers(C), Challenger).


%%%%%%%%%%%% Acces dans une liste %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vector_access(1,[X|_],X) :- !.
vector_access(N,[H|_],X) :- H == X, !.
vector_access(N,[_|R],X) :- vector_access(N-1,R,X).


%%%%%%%%%%%% Modifieur d'une liste %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vector_incr(V, Ind) :- vector_access(Ind, V, Value), replace(V, Ind, Value+1).


%%%%%%%%%%%% Point d'entrée du module %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Deux signatures : une pas défaut, et avec en parametre le nombre de parties à jouer pour chaque confrontation
start_evaluation :- P1 is 0, P2 is 0, evaluation_loop(P1,P2,100).
start_evaluation(N) :- P1 is 0, P2 is 0, evaluation_loop(P1,P2,N).


%%%%%%%%%%%% Parcours de l'ensemble des confrontations %%%%%%%%%%%%%
evaluation_loop(P1, 3) :- !.                            % Tous les challengers sont passés
evaluation_loop(3, P2) :- evaluation_loop(0, P2+1), !.  % Passage au challenger suivant pour P2 (retour à 0 pour P1)
evaluation_loop(P1,P2,N) :- 
	versus_loop(C1, C2, N),
	evaluation_loop(P1+1, P2, N).                       % Passage au challenger suivant pour P1


%%%%%%%%%%%% Réalisation des N parties pour une confrontation %%%%%%
versus_loop(P1,P2,1) :- !.
versus_loop(P1,P2,N) :- 
	challenger(P1, C1), 
	challenger(P2, C2), 
	play(C1, C2, Winner), 
	refresh_score(P1,P2,Winner),  
	versus_loop(P1, P2, N-1).


%%%%%%%%%%%% Mise à jour des scores %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
refresh_score(P1,P2,Winner) :- 
	challenger(N, Winner), 
	P1 == N, 
	add_one_to_score(P1,P2,Winner).

	
%%%%%%%%%%%% Ajoute un au score du challenger P1 contre P2 %%%%%%%%%
add_one_to_score(P1,P2) :- 
	victory_rates(V),
	vector_incr(V,P2+(P1*3)).