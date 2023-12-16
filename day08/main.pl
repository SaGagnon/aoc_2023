:- use_module(library(rbtrees)).
:- use_module(library(thread)).
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

find_zzz_(_, _, _, Node, NbSteps, NbSteps) :-
    (   part(2)
    ->  end_with(z, Node)
    ;   Node=zzz
    ).
find_zzz_(G, Is, [NextI|NextIs], CurNode, NbSteps0, NbSteps) :-
    read_instruction(G, NextI, CurNode, NextNode),
    NbSteps1 #= NbSteps0 + 1,
    find_zzz_(G, Is, NextIs, NextNode, NbSteps1, NbSteps).
find_zzz_(G, Is, [], CurNode, NbSteps0, NbSteps) :-
    find_zzz_(G, Is, Is, CurNode, NbSteps0, NbSteps).

%% solve

solve1(NbSteps) :-
    abolish(part/1),
    assertz(part(1)),
    phrase_from_file(map(Is, Neis), 'input.txt'),
    graph_init(Neis, G),
    find_zzz(G, Is, NbSteps).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end_with(C, N) :- atom_codes(N, [_,_,X]), char_code(C, X).

starting_nodes(G, Ns) :-
    findall(N, (graph_node(G, N), end_with(a, N)), Ns).

find_zzzs(G, Is, NbSteps) :-
    starting_nodes(G, Ns),
    maplist({G,Is}/[N, NbStep]>>find_zzz_(G, Is, Is, N, 0, NbStep), Ns, NbSteps).

%% solve
solve2(NbSteps) :-
    abolish(part/1),
    assertz(part(2)),
    phrase_from_file(map(Is, Neis), 'input.txt'),
    graph_init(Neis, G),
    find_zzzs(G, Is, NbSteps).

% Il faut trouver le LCM du résultat
