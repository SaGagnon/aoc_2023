%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

games([G|Gs]) --> game(G), sequence(game, Gs).

game(game(Id, Ss)) --> `Game`, blank, number(Id), `:`, blank, sets(Ss), eol.

sets([S|Ss]) --> set(S), `;`, blank, sets(Ss).
sets([S]) --> set(S).

set([C|Cs])  --> cubes(C), `,`, blank, set(Cs).
set([C]) --> cubes(C).

cubes(C) --> number(N), blank, color(Col_),
             { atom_codes(Col, Col_),
               C =.. [Col, N] }.     

color(`red`)   --> `red`   .
color(`green`) --> `green` .
color(`blue`)  --> `blue`  .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enough_cubes(red(N))   :- N #=< 12.
enough_cubes(green(N)) :- N #=< 13.
enough_cubes(blue(N))  :- N #=< 14.

good_game(game(_, Ss)) :-
    append(Ss, Flat),
    maplist(enough_cubes, Flat).

solve1(S) :-
    phrase_from_file(games(G0s), 'input.txt'),
    include(good_game, G0s, Gs),
    maplist(arg(1), Gs, Ids),
    sum_list(Ids, S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

game_power(game(_, Ss), P) :-
    append(Ss, Flat),
    maplist([C, Col-N]>>(C=..[Col,N]), Flat, Pairs0),
    keysort(Pairs0, Pairs1),
    group_pairs_by_key(Pairs1, Pairs),
    pairs_values(Pairs, Vs),
    maplist(max_list, Vs, Maxs),
    foldl([X,Y,Z]>>(Z is X*Y), Maxs, 1, P).

solve2(S) :-
    phrase_from_file(games(Gs), 'input.txt'),
    maplist(game_power, Gs, Ps),
    sum_list(Ps, S).
    
    
