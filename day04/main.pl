%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

card(card(Id, Ws, Ys)) --> `Card`, blanks, number(Id), `:`, numbers(Ws), ` | `, numbers(Ys), eol.

cards([C|Cs]) --> card(C), cards(Cs).
cards([C]) --> card(C).

numbers([N|Ns]) --> blanks, number(N), numbers(Ns).
numbers([N]) --> blanks, number(N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

card_points(card(_, W0s, Y0s), P) :-
    sort(W0s, Ws),
    sort(Y0s, Ys),
    ord_intersection(Ws, Ys, Is),
    length(Is, L),
    N #= L - 1,
    (   N #>= 0
    ->  P #= 2 ^ N
    ;   P = 0
    ).

solve1(S) :-
    phrase_from_file(cards(Cs), 'input.txt'),
    maplist(card_points, Cs, Ps),
    sum_list(Ps, S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

card_init(card(Id, W0s, Y0s), card(copies(1), wins(W))) :-
    sort(W0s, Ws),
    sort(Y0s, Ys),
    ord_intersection(Ws, Ys, Is),
    length(Is, W).

pop_card([card(copies(N), wins(W))|Cs0], Cs, Nb0, Nb) :-
    Nb #= Nb0 + N,
    inc_cards(W, N, Cs0, Cs).

inc_cards(0, _, Cs, Cs).
inc_cards(W0, Inc,
          [card(copies(N0), wins(W_))|Cs0],
          [card(copies(N),  wins(W_))|Cs]) :-
    N #= N0 + Inc,
    W #= W0 - 1,
    inc_cards(W, Inc, Cs0, Cs).

pop_cards(Cs, Nb) :-
    pop_cards_(Cs, 0, Nb).

pop_cards_([], Nb, Nb).
pop_cards_(Cs0, Nb0, Nb) :-
    pop_card(Cs0, Cs, Nb0, Nb1),
    pop_cards_(Cs, Nb1, Nb).
    
solve2(S) :-
    phrase_from_file(cards(Cs0), 'input.txt'),
    maplist(card_init, Cs0, Cs),
    pop_cards(Cs, S).
