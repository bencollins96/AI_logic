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
  query_world( agent_current_position, [Agent,P]),
  solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos,[]),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).
%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*

%Base case -> supplies the task, checks if Current is a goal node, produces depth
% the reverse path, cost and depth?! New pos for backtracking, and visited = list of visited nodes.
solve_task_bt(Task,[Current|Agenda],Depth,RPath,[cost(Cost),depth(Depth)],NewPos,Visited) :-  
  achieved(Task,Current,RPath,Cost,NewPos).

%%% go task %%%
solve_task_bt(go(GoalPos),[Current|Agenda],D,RR,Cost,NewPos,Visited) :-
  
  %Obtains the current node along with its cost and the path to get there.
  Current = [c(F,G,P)|RPath],
  D1 is D +1,  

  %Set the distance travelled to G+1 for all children of the current node.
  G1 is G+1, 

  %Find all the children of the current node... but they cannot be in visited list.
  %If there are no such children and therefore setof fails -> leave the agenda unchanged.
  
  (setof([c(F1,G1,Pos1),Pos1|RPath], search1(P,Pos1,Pos1,_,F1,GoalPos,G1,Visited), Children) 
  -> merge(Agenda,Children,NewAgenda)
  ; NewAgenda = Agenda
  ),

  remove_items(NewAgenda,Visited,MinusAgenda),
  
  D1 is D+1,
  solve_task_bt(go(GoalPos),MinusAgenda,D1,RR,Cost,NewPos, [P|Visited]). 

solve_task_bt(find(o(X)),[Current|Agenda],D,RR,Cost,NewPos,Visited) :-
  
  %Obtains the current node along with its cost and the path to get there.
  Current = [c(F,G,P)|RPath],
  D1 is D +1,  

  %Set the distance travelled to G+1 for all children of the current node.
  G1 is G+1, 

  %Find all the children of the current node... but they cannot be in visited list.
  %If there are no such children and therefore setof fails -> leave the agenda unchanged.
  (setof([c(G1,G1,Pos1),Pos1|RPath], search2(P,Pos1,Pos1,_,F1,_,G1,Visited), Children) 
  -> merge(Agenda,Children,NewAgenda)
  ; NewAgenda = Agenda
  ),

  remove_items(NewAgenda,Visited,MinusAgenda),
  
  D1 is D+1,
  solve_task_bt(find(o(X)),MinusAgenda,D1,RR,Cost,NewPos, [P|Visited]). 

solve_task_bt(find(c(X)),[Current|Agenda],D,RR,Cost,NewPos,Visited) :-
  
  %Obtains the current node along with its cost and the path to get there.
  Current = [c(F,G,P)|RPath],
  D1 is D +1,  

  %Set the distance travelled to G+1 for all children of the current node.
  G1 is G+1, 

  %Find all the children of the current node... but they cannot be in visited list.
  %If there are no such children and therefore setof fails -> leave the agenda unchanged.
  (setof([c(G1,G1,Pos1),Pos1|RPath], search2(P,Pos1,Pos1,_,F1,_,G1,Visited), Children) 
  -> merge(Agenda,Children,NewAgenda)
  ; NewAgenda = Agenda
  ),
  
  remove_items(NewAgenda,Visited,MinusAgenda),
  
  D1 is D+1,
  solve_task_bt(find(c(X)),MinusAgenda,D1,RR,Cost,NewPos, [P|Visited]). 




  %Remove Agendas going to places already seen.
  remove_items(NewAgenda,[],NewAgenda).
  remove_items(NewAgenda,[H|Tail],MinusAgenda):-
    delete(NewAgenda,[c(_,_,H)|_],MinusAgenda),
    remove_items(MinusAgenda,Tail,NewMinus).




%%% find task %%% 
% solve_task_bt(find(o(X)),[Current|Agenda],D,RR,Cost,NewPos,Visited) :-
  
%   Current = [c(F,G,P)|RPath],
%   (memberchk(P,Visited)
%     -> solve_task_bt(find(o(X)),Agenda,G,RR,Cost,NewPos, Visited)
%   ;true),
%   G1 is G+1, 

%   (
%   findall([c(G1,G1,Pos1),Pos1|RPath], search2(P,Pos1,Pos1,_,F1,_,G1,Visited), Children)
%   -> append(Agenda,Children,NewAgenda) %Swapping the two results in shortest path? but why..
%   ; NewAgenda   = Agenda
%   ),

%   D1 is D+1,
%   solve_task_bt(find(o(X)),NewAgenda,G1,RR,Cost,NewPos, [P|Visited]). 

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

search1(F,N,N,1,F1,GoalPos,G1,Visited) :-
  map_adjacent(F,N,empty),
  \+ memberchk(N,Visited),
  map_distance(N,GoalPos,H), 
  F1 is G1 + H.

search2(F,N,N,1,F1,_,G1,Visited) :-
  map_adjacent(F,N,empty),
  \+ memberchk(N,Visited),
  F1 is G1.

search(F,N,N,1) :-
  map_adjacent(F,N,empty).
