:- use_module(library(rbtrees)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instructions([I|Is]) --> instruction(I), sequence(instruction, Is), eol.

instruction(r) --> `R` .
instruction(l) --> `L` .

node(N) --> [A,B,C], { N0 = [A,B,C], atom_codes(N1, N0), downcase_atom(N1, N) }.

nei(N-e(L,R)) --> node(N), ` = (`, node(L), `, `, node(R), `)`, eol.

neis([Nei|Neis]) --> nei(Nei), sequence(nei, Neis).

map(Is, Neis) --> instructions(Is), eol, neis(Neis).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Graph

graph_init(Neis, G) :-
    keysort(Neis, Neis1),
    ord_list_to_rbtree(Neis1, G).

graph_node_left(G, N, L)  :- rb_lookup(N, e(L,_), G).
graph_node_right(G, N, R) :- rb_lookup(N, e(_,R), G).
graph_node(G, N)          :- rb_in(N, _, G).

%% Instructions

read_instruction(G, l, N, L) :- graph_node_left(G, N, L).
read_instruction(G, r, N, R) :- graph_node_right(G, N, R).

%% find_zzz

find_zzz(G, Is, NbSteps) :-
    find_zzz_(G, Is, Is, aaa, 0, NbSteps).

find_zzz_(_, _, _, zzz, NbSteps, NbSteps).
find_zzz_(G, Is, [NextI|NextIs], CurNode, NbSteps0, NbSteps) :-
    dif(CurNode, zzz),
    read_instruction(G, NextI, CurNode, NextNode),
    NbSteps1 #= NbSteps0 + 1,
    find_zzz_(G, Is, NextIs, NextNode, NbSteps1, NbSteps).
find_zzz_(G, Is, [], CurNode, NbSteps0, NbSteps) :-
    find_zzz_(G, Is, Is, CurNode, NbSteps0, NbSteps).

%% solve

solve1(NbSteps) :-
    phrase_from_file(map(Is, Neis), 'input.txt'),
    graph_init(Neis, G),
    find_zzz(G, Is, NbSteps).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ne fonctionne pas. Il faut utiliser une autre méthode qui n'énumère
% pas explicitement.

%% predicates for several nodes

end_with(C, N) :- atom_codes(N, [_,_,X]), char_code(C, X).

starting_nodes(G, Ns) :-
    findall(N, (graph_node(G, N), end_with(a, N)), Ns).

end_state(Ns) :-
    maplist(end_with(z), Ns).

read_instruction_allnodes(G, l, Ns, Ls) :- !, maplist(graph_node_left(G), Ns, Ls).
read_instruction_allnodes(G, r, Ns, Rs) :- !, maplist(graph_node_right(G), Ns, Rs).

%% find_zzzs

find_zzzs(G, Is, NbSteps) :-
    starting_nodes(G, Ns),
    find_zzzs_(G, Is, Is, Ns, 0, NbSteps).

find_zzzs_(_, _, _, Nodes, NbSteps, NbSteps) :-
    end_state(Nodes).
find_zzzs_(G, Is, [NextI|NextIs], CurNodes, NbSteps0, NbSteps) :- !,
    \+ end_state(CurNodes),
    $(read_instruction_allnodes(G, NextI, CurNodes, NextNodes)),
    NbSteps1 #= NbSteps0 + 1,
    (   NbSteps1 mod 10000 #= 0
    ->  print(NbSteps1), nl
    ;   true
    ),
    find_zzzs_(G, Is, NextIs, NextNodes, NbSteps1, NbSteps).
find_zzzs_(G, Is, [], CurNodes, NbSteps0, NbSteps) :-
    find_zzzs_(G, Is, Is, CurNodes, NbSteps0, NbSteps).

%% solve

solve2(NbSteps) :-
    phrase_from_file(map(Is, Neis), 'input.txt'),
    graph_init(Neis, G),
    find_zzzs(G, Is, NbSteps).
