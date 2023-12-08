%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

card_strength(C, S) :-
    (   part(1)
    ->  nth1(S0, `23456789TJQKA`, C)
    ;   nth0(S0, `J23456789TQKA`, C)
    ),
    S #= S0 + 1.

h(h(Hand, Bid)) --> nonblanks(Hand_), blank, number(Bid), eol,
                          { maplist(card_strength, Hand_, Hand) }.

hs([H|Hs]) --> h(H), sequence(h, Hs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hand_type(H, T) :-
    msort(H, H1),
    clumped(H1, H2),
    pairs_values(H2, H3),
    msort(H3, H4),
    reverse(H4, H5),
    hand_type_(H5, T).

hand_type_([5], 7). % Five of a kind
hand_type_([4, 1], 6). % Four of a kind
hand_type_([3, 2], 5). % Full house
hand_type_([3, 1, 1], 4). % Three of a kind
hand_type_([2, 2, 1], 3). % Two pair
hand_type_([2, 1, 1, 1], 2). % One pair
hand_type_([1, 1, 1, 1, 1], 1). % High card

hand_strength(H, [T|H]) :-
    hand_type(H, T).

hand__strength_bid(h(H, Bid), S-Bid) :-
    (   part(1)
    ->  hand_strength(H, S)
    ;   hand_strength2(H, S)
    ).

total_winnings(Bs, W) :-
    total_winnings_(Bs, 1, W).

total_winnings_([], _, 0).
total_winnings_([B|Bs], R, W) :-
    W #= R*B + W0,
    R0 #= R + 1,
    total_winnings_(Bs, R0, W0).

:- dynamic part/1.

solve(W) :-
    phrase_from_file(hs(Hs), 'input.txt'),
    maplist(hand__strength_bid, Hs, SB0s),
    keysort(SB0s, SBs),
    pairs_values(SBs, Bs),
    total_winnings(Bs, W).

solve1(W) :-
    abolish(part/1),
    assertz(part(1)),
    solve(W).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

replace(_, [], _, []).
replace(X, [X|XList], Y, [Y|YList]) :- replace(X, XList, Y, YList).
replace(X, [H|XList], Y, [H|YList]) :- dif(H,X), replace(X, XList, Y, YList).

hand_strength2(H, S) :-
    replace(1, H, _, JokerH),
    term_variables(JokerH, Jokers),
    Jokers ins 2..13,
    findall([T|H], (label(Jokers), hand_strength(JokerH, [T|_])), Ss0),
    sort(Ss0, Ss),
    last(Ss, S).

solve2(W) :-
    abolish(part/1),
    assertz(part(2)),
    solve(W).
