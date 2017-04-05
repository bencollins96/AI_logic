candidate_number(12345).

solve_task(Task,Cost) :-
  ( part(1) -> solve_task_1_3(Task, Cost)
  ; part(3) -> solve_task_1_3(Task, Cost)
  ; part(4) -> solve_task_4(Task, Cost)
  ).

%%%%%%%%%% Part 1 & 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_1_3(Task,Cost) :-
  agent_current_position(oscar,P),
  solve_task_bt(Task,[[c(0,0,P),P]],0,R,Cost,_NewPos,[]),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  agent_do_moves(oscar,Path).
%%%%%%%%%% Part 1 & 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%










%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_4(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).
%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*

solve_task_bt(Task,[Current|Agenda],Depth,RPath,[cost(Cost),depth(Depth)],NewPos,SqList) :-
  achieved(Task,Current,RPath,Cost,NewPos).

%%% go task %%%
solve_task_bt(go(GoalPos),[Current|Agenda],D,RR,Cost,NewPos,SqList) :-
  
  Current = [c(F,G,P)|RPath],  
  G1 is G+1, 

  (
  setof([c(FL,G1,Pos1),Pos1|RPath], search1(P,Pos1,Pos1,_,FL,GoalPos,G1,SqList), Children)
  -> append(Children,Agenda,NewAgenda)
  ; NewAgenda   = Agenda
  ),
  
  D1 is D+1,
  solve_task_bt(go(GoalPos),NewAgenda,D1,RR,Cost,NewPos, [P|SqList]). 

%%% find task %%%
solve_task_bt(find(o(X)),[Current|Agenda],D,RR,Cost,NewPos,SqList) :-
  
  Current = [c(F,G,P)|RPath],  
  G1 is G+1, 

  (
  setof([c(G1,G1,Pos1),Pos1|RPath], search2(P,Pos1,Pos1,_,SqList), Children)
  -> append(Children,Agenda,NewAgenda)
  ; NewAgenda   = Agenda
  ),
  
  D1 is D+1,
  solve_task_bt(find(o(X)),NewAgenda,D1,RR,Cost,NewPos, [P|SqList]). 

achieved(go(Exit),Current,RPath,Cost,NewPos) :-
  Current = [c(Total,Cost,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,Travel,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

search1(F,N,N,1,F1,GoalPos,G1,SQList) :-
  map_adjacent(F,N,empty),
  \+ memberchk(N,SQList),
  map_distance(N,GoalPos,H),  
  F1 is G1 + H.

search2(F,N,N,1,SQList) :-
  map_adjacent(F,N,empty),
  \+ memberchk(N,SQList).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).
