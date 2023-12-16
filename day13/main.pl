%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ash('.') --> `.` .
rock('#') --> `#` .

char(C) --> ash(C) | rock(C).

line([C|Cs]) --> char(C), sequence(char, Cs), eol.

pattern([L|Ls]) --> line(L), sequence(line, Ls), eol.

patterns([P|Ps]) --> pattern(P), sequence(pattern, Ps), eos.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reflec(Lines, Nb) :- reflec_left(Lines,Nb).
reflec(Lines, Nb) :- \+ reflec_left(Lines,Nb),
                     reflec_up(Lines,Nb0),
                     Nb #= Nb0 * 100.

reflec_up(Lines, Nb) :-
    transpose(Lines, TLines),
    reflec_left(TLines, Nb).

reflec_left(Lines, Nb) :-
    same_length(Lines, Rs),
    maplist(=([]), Rs),
    reflec_(Lines, Rs, Lefts),
    nth0(0, Lefts, Left),
    length(Left, Nb).

reflec_(Ls, Rs, Lefts) :-
    nth0(0, Ls, L),
    nth0(0, Rs, R),
    length(L, L1),
    length(R, L2),
    L1 #> 0,
    L2 #> 0,
    maplist(eq, Ls, Rs),
    Lefts=Rs.
reflec_(Ls, Rs, Lefts) :-
    maplist([[H|L], R, L, [H|R]]>>true, Ls, Rs, Ls1, Rs1),
    reflec_(Ls1, Rs1, Lefts).

eq([], _).
eq(_, []).
eq([H|L], [H|R]) :-
    eq(L, R).

solve1(S) :-
    phrase_from_file(patterns(Ps), 'input.txt'),
    maplist(reflec, Ps, Nbs),
    sum_list(Nbs, S).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mat_flip(Lines0, Lines) :-
    matrix_dimensions(Lines0, NbRows, NbCols),
    between(1, NbRows, J),
    between(1, NbCols, I),
    nth1(J, Lines0, Line0, LinesRest),
    nth1(J, Lines, Line, LinesRest),
    nth1(I, Line0, X, LineRest),
    nth1(I, Line, Y, LineRest),
    dif(X,Y),
    member(Y,['.','#']).

reflec_flip(Lines, Nb) :-
    mat_flip(Lines, Lines1),
    reflec(Lines, Nb0),
    dif(Nb0, Nb),
    reflec(Lines1, Nb).
    
solve2(S) :-
    phrase_from_file(patterns(Ps), 'input.txt'),
    maplist(reflec_flip, Ps, Nbs),
    sum_list(Nbs, S).
