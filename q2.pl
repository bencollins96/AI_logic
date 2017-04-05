find_identity(A):-
    setof(A, possible_actor(A), Possibles),
    find_identity(A,Possibles).

find_identity(A,[Inter]):-
    actor(Inter),
    A = Inter,!.

find_identity(A,Possibles):-
    setof(A,possible_actor(A),NewPossibles),
    intersection(Possibles, NewPossibles, Inter),  %write
    find_identity(A,Inter).

possible_actor(A):- 
    actor(A),
    setof([A|Linklist], collector(Linklist,A),Alllinks),
    get_link_list(Alllinks).

collector(Linklist,A):-
    setof(L, (wp(A,WT),wt_link(WT,L)), Linklist).

get_link_list([H|T]):- 
    test(H).

test([H|T]):-
    agent_ask_oracle(oscar,o(1),link,Testlink),
    memberchk(Testlink,T).