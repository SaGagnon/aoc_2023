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

%% bisect_enum

bisect_enum(Start, End, N) :-
    Width #= End - Start + 1,
    Numerator in 1..Width,
    Denominator in 1..Width,
    Numerator #=< Denominator,
    Numerator mod 2 #= 1,
    Pow in 0..sup,
    Denominator #= 2 ^ Pow,
    label([Denominator, Numerator]),
    N is Start + Width * Numerator // Denominator - 1.

%% binary_search_leftmost

binary_search_leftmost(AccessIdx, CompareVal, LeftIdx, RightIdx, TargetVal, TargetIdx) :-
    RightIdx1 = RightIdx + 1,
    binary_search_leftmost_(AccessIdx, CompareVal, LeftIdx, RightIdx1,
                            TargetVal, TargetIdx),
    TargetIdx #< RightIdx1,
    call(AccessIdx, TargetIdx, TargetVal).

binary_search_leftmost_(AccessIdx, CompareVal, LeftIdx, RightIdx, TargetVal, TargetIdx) :-
    LeftIdx #< RightIdx,
    M #= (LeftIdx + RightIdx) // 2,
    call(AccessIdx, M, MVal),
    call(CompareVal, Order, MVal, TargetVal),
    (   Order = <,
        LeftIdx1 #= M + 1,
        RightIdx1 #= RightIdx
    ;   dif(Order, <),
        LeftIdx1 #= LeftIdx,
        RightIdx1 #= M
    ),
    binary_search_leftmost_(AccessIdx, CompareVal, LeftIdx1, RightIdx1,
                            TargetVal, TargetIdx).
binary_search_leftmost_(_, _, LeftIdx, RightIdx, _, LeftIdx) :-
    LeftIdx #>= RightIdx.

:- begin_tests(binary_search_leftmost).

bin_search_leftmost_numbers(List, TargetVal, TargetIdx) :-
    AccessIdx = {List}/[Index, Elem]>>nth0(Index, List, Elem),
    length(List, N0),
    N #= N0 - 1,
    binary_search_leftmost(AccessIdx, zcompare, 0, N, TargetVal, TargetIdx).

test(n1, [nondet]) :-
    bin_search_leftmost_numbers([1,1,2,3], 1, 0).

test(n2, [nondet]) :-
    bin_search_leftmost_numbers([1,1,2,3], 2, 2).

test(n3, [nondet]) :-
    bin_search_leftmost_numbers([1,1,2,3], 3, 3).

test(n4, [nondet]) :-
    bin_search_leftmost_numbers([1,1,2,3,3,3,3,3], 3, 3).

test(n5, [nondet]) :-
    bin_search_leftmost_numbers([1,1,2,3,4,4,4,4], 4, 4).

test(n6, [nondet]) :-
    bin_search_leftmost_numbers([1,1,2,3,4,4,4,4,5,5,5,5], 4, 4).

:- end_tests(binary_search_leftmost).

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
    bisect_enum(0, Time, Hold),
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
    % MinHold (inclusif)
    binary_search_leftmost(race_status(R), comp_min_hold,
                           0, WinHold, win, MinHold),
    % MaxHold (exclusif)
    binary_search_leftmost(race_status(R), comp_max_hold,
                           WinHold, Time, loss, MaxHold),
    HoldRange #= MaxHold - MinHold,
    label([MinHold, MaxHold]).

