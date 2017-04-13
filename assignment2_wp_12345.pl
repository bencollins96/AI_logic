% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)

%Main function - Identifies actors that it may possibly be.
find_identity(A):-
    setof(A, actor(A), Possibles),
    find_identity(A,Possibles,1).

%Sort of a base case. If the list of possible actors is only 1 actor, then it tests to see 
% if it is indeed an actor, then unifies A with actor and cuts to avoid further backtracking 
% i.e repeating the same actor over and over._
find_identity(A,[Inter],_):-
    actor(Inter),
    A = Inter,!.

%Here is the meat and veg of the 'algorithm'.
%This identifes all the new possible actors, then finds which are in common between
%old possible and new possible. Then calls itself. Potentiall the find_link function can go here and be
%passed as an input to possible_actor? probably makes more sense.

find_identity(A,Possibles,Num):-
    agent_current_energy(oscar,E),

    %if energy is less than 60 topup. else continue on your journey
    (E <60
    -> solve_task(find(c(X)),C), agent_topup_energy(oscar,c(X)) 
    ;true),

    find_link(Testlink,Num),
    Num1 is Num+1,
    writeln(Testlink),
    setof(A,possible_actor(A,Testlink),NewPossibles),
    intersection(Possibles, NewPossibles, Inter),  
    find_identity(A,Inter,Num1).


%THis find all possible actors.  finds set of all actors, then finds
% set of all actors and their links in the form [[Actor1|Links_for_Actor1],[Actor2|...]..].
%Then it it takes that list of alllinks and passes to to get_link_list. 
possible_actor(A,Testlink):- 
    actor(A),
    setof([A|Linklist], collector(Linklist,A),Alllinks),
    check(Alllinks,Testlink).

collector(Linklist,A):-
    setof(L, (wp(A,WT),wt_link(WT,L)), Linklist).

%Selects one element of the list. i.e one actor|link pair.
check([[H|Links]|T],Testlink):- 
    memberchk(Testlink,Links).



%Here it splits up the actor|link pair, find a link, and iteratively?! (not! but it should be) tests to see if the link
% is contained in the set of links. I see, not really the be
    


%%%%%%%%%%%%%%% Just do sequentially at least then it works..
find_link(Testlink,Num):-
    solve_task(find(o(Num)),C),    
    agent_ask_oracle(oscar,o(Num),link,Testlink).










%Lets think about it. each time test is called it should probably call some other function
%   sayyyy find_oracle() which returns a testlink?