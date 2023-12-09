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

%% Graph & instructions

graph_init(Neis, G) :- foldl(graph_add, Neis, [], G).
graph_add(N-e(L,R), G0, [N-e(L,R)|G0]).

graph_node_left(G, N, L)  :- select(N-e(L,_), G, _).
graph_node_right(G, N, R) :- select(N-e(_,R), G, _).

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
    find_zzz(G, Is, NbSteps),
    *NbSteps #> 1084.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve2(S) :-
    phrase_from_file(S, 'small_input.txt').
