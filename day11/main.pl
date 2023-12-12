%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% state(I,J,Gs).

void(   s(I0,J,Gs), s(I,J,Gs))           --> `.`,  { I #= I0 + 1 }.
galaxy( s(I0,J,Gs), s(I,J,[g(I0,J)|Gs])) --> `#`,  { I #= I0 + 1 }.
newline(s(_,J0,Gs), s(0,J,Gs))           --> `\n`, { J #= J0 + 1 }.

char(S0, S) --> void(S0, S) ; galaxy(S0, S) ; newline(S0, S).

galaxies(S0, S) --> char(S0, S1), galaxies(S1, S).
galaxies(S0, S) --> char(S0, S).

galaxies(Gs) --> galaxies(s(0,0,[]), s(_,_,Gs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Expansion

empty_idxs(Gs, Dim, Idxs) :-
    (   Dim=col,  D=1
    ;   Dim=line, D=2
    ),
    maplist(arg(D), Gs, GalaxyIdxs),
    max_list(GalaxyIdxs, GalaxyIdxMax),
    numlist(0, GalaxyIdxMax, AllIndexes),
    subtract(AllIndexes, GalaxyIdxs, Idxs).
    
expand(Gs0, Gs) :-
    empty_idxs(Gs0, col, Is),
    empty_idxs(Gs0, line, Js),
    maplist(galaxy_push(Is, Js), Gs0, Gs).

galaxy_push(EmptyLines, EmptyCols, g(I0, J0), g(I,J)) :-
    galaxy_push_(I0, EmptyLines, 0, PushLine),
    galaxy_push_(J0, EmptyCols, 0, PushCol),
    I #= I0 + PushLine,
    J #= J0 + PushCol.

galaxy_push_(_, [], P, P).
galaxy_push_(GI, [EI|_], P, P) :-
    GI #< EI.
galaxy_push_(GI, [EI|EIs], P0, P) :-
    GI #> EI,
    P1 #= P0 + 999999, % Part 2
    galaxy_push_(GI, EIs, P1, P).

%% Pairs sum

galaxy_pair(Gs, G0, G1) :-
    member(G0, Gs),
    member(G1, Gs),
    G0 = g(I0, J0),
    G1 = g(I1, J1),
    I0 #< I1 #\/ ( I0 #= I1 #/\ J0 #< J1).

galaxy_pair_dist(g(I0, J0), g(I1, J1), Dist) :-
    Dist #= abs(I1-I0) + abs(J1-J0).

galaxy_pairs_sum(Gs, S) :-
    findall(G0-G1, galaxy_pair(Gs, G0, G1), Pairs),
    pairs_keys_values(Pairs, G0s, G1s),
    maplist(galaxy_pair_dist, G0s, G1s, Dists),
    sum_list(Dists, S).

solve1(S) :-
    phrase_from_file(galaxies(Gs0), 'input.txt'),
    expand(Gs0, Gs),
    galaxy_pairs_sum(Gs, S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve2(_) :-
    phrase_from_file(_, 'small_input.txt').
