%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

numbers([N|Ns]) --> number(N), ` `, numbers(Ns).
numbers([N]) --> number(N).

line(Ns) --> numbers(Ns), eol.

lines([L|Ls]) --> line(L), sequence(line, Ls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

diffs([_], []).
diffs([A,B|Ls], [D|Ds]) :-
    D #= B - A,
    diffs([B|Ls], Ds).

diffs_until_0(Ds0, Ds) :-
    diffs_until_0_([Ds0], Ds).

diffs_until_0_([Ds0|Rest], Ds) :-
    (   maplist(=(0), Ds0)
    ->  [Ds0|Rest] = Ds
    ;   diffs(Ds0, Ds1),
        diffs_until_0_([Ds1,Ds0|Rest], Ds)
    ).

extrapolate(LL, V) :-
    foldl(extrapolate_, LL, 0, V).

extrapolate_(L, N0, N) :-
    (   part(1)
    ->  last(L, N1),
        N #= N0 + N1
    ;   L=[N1|_],
        N #= N1 - N0
    ).

line_extrapolation(L, V) :-
    diffs_until_0(L, LL),
    extrapolate(LL, V).

solve(P, S) :-
    abolish(part/1),
    assertz(part(P)),
    phrase_from_file(lines(Ls), 'input.txt'),
    maplist(line_extrapolation, Ls, Vs),
    sum_list(Vs, S).    

solve1(S) :-
    solve(1, S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve2(S) :-
    solve(2, S).
