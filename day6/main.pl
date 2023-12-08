%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n(T) --> blanks, number(T).

times([T|Ts]) --> `Time:`, n(T), sequence(n, Ts), eol.
records([R|Rs]) --> `Distance:`, n(R), sequence(n, Rs), eol.

sheet(sheet(Ts, Rs)) --> times(Ts), records(Rs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sheet_races(sheet(Ts, Rs), Races) :-
    transpose([Ts, Rs], Races0),
    maplist([[T, R], race(T,R)]>>true, Races0, Races).

race_win(race(Time, OldRecord), Hold) :-
    [Hold, Dur] ins 0..Time,
    Time      #= Hold + Dur,
    NewRecord #= Hold * Dur,
    NewRecord #> OldRecord.

race_nways(R, N) :-
    race_win(R, MinHold),
    race_win(R, MaxHold),
    once(labeling([min(MinHold)], [MinHold])),
    once(labeling([max(MaxHold)], [MaxHold])),
    N #= MaxHold - MinHold + 1.

solve(File, M) :-
    phrase_from_file(sheet(S), File),
    sheet_races(S, Rs),
    maplist(race_nways, Rs, Ns),
    mult_list(Ns, M).
    
solve1(M) :-
    solve('input.txt', M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generic utility functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% halving_enum(+Start, +Size, -N).
%
% Énumération dans l'intervalle [Start,Start+Size[ avec les fractions
% suivantes pour l'équation Start + Size * Fraction.
%
%    1/1, 1/2, 1/4, 3/4, 1/8, 3/8, 5/8, 7/8, 1/16, ...
%
%?- halving_enum(1,8,N).
%@ N = 8 ;
%@ N = 4 ;
%@ N = 2 ;
%@ N = 6 ;
%@ N = 1 ;
%@ N = 3 ;
%@ N = 5 ;
%@ N = 7.
halving_enum(Start, Size, N) :-
    Numerator in 1..Size,
    Denominator in 1..Size,
    Numerator #=< Denominator,
    Numerator mod 2 #= 1,
    Pow in 0..sup,
    Denominator #= 2 ^ Pow,
    label([Denominator, Numerator]),
    N is Start + Size * Numerator // Denominator - 1.

%% binary_search_leftmost(:Access, :Cmp, +Start, +Size, +T, -T_Idx).
%
% Recherche binaire de T dans les indexes de l'interval continu
% [Start,Start+Size[. La fonction Access(+Idx, -T) est utilisée pour
% obtenir les valeurs de chaque élément dans l'interval. La fonction
% Cmp(-Order, +A, +B) est utilisé pour comparer les valeurs entre
% elles (voir signature fonction zcompare).
%
% Si plusieurs éléments sont égaux, l'index de premier élément est
% retourné.
%
% L'implémentation est basé sur le pseudo-code "Procedure for finding
% the leftmost element" de la page
% https://en.wikipedia.org/wiki/Binary_search_algorithm.

binary_search_leftmost(Access, Cmp, Start, Size, T, T_Idx) :-
    binary_search_leftmost_(Access, Cmp, Start, Size, T, T_Idx),
    T_Idx #< Size,
    call(Access, T_Idx, T).

binary_search_leftmost_(Access, Cmp, L, R, T, T_Idx) :-
    L #< R,
    M #= (L + R) // 2,
    call(Access, M, MVal),
    call(Cmp, Order, MVal, T),
    (   Order = <,
        L1 #= M + 1,
        R1 #= R
    ;   dif(Order, <),
        L1 #= L,
        R1 #= M
    ),
    binary_search_leftmost_(Access, Cmp, L1, R1, T, T_Idx).

binary_search_leftmost_(_, _, L, R, _, L) :-
    L #>= R.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Solving
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

race_status(R, Hold, Status) :-
    (   race_win(R, Hold)
    ->  Status = win
    ;   Status = loss
    ).

bisect_race_win(R, Hold) :-
    R = race(Time, _),
    halving_enum(0, Time, Hold),
    race_win(R, Hold).

comp_min_hold(<, loss, win).
comp_min_hold(>, win, loss).
comp_min_hold(=, X, X).

comp_max_hold(<, win, loss).
comp_max_hold(>, loss, win).
comp_max_hold(=, X, X).
                           
solve2(HoldRange) :-
    phrase_from_file(sheet(S), 'input2.txt'),
    sheet_races(S, [R]),
    R = race(Time, _),
    % Random win.
    once(bisect_race_win(R, WinHold)),
    % MinHold
    WinHoldNext #= WinHold + 1,
    binary_search_leftmost(race_status(R), comp_min_hold,
                           0, WinHoldNext, win, MinHold),
    % MaxHold
    binary_search_leftmost(race_status(R), comp_max_hold,
                           WinHold, Time, loss, MaxHold),
    HoldRange #= MaxHold - MinHold,
    label([MinHold, MaxHold]).

