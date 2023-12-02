%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

l(X), Rest --> X, {X=[_|Rest]}.

d(1) --> l(`one`).
d(2) --> l(`two`).
d(3) --> l(`three`).
d(4) --> l(`four`).
d(5) --> l(`five`).
d(6) --> l(`six`).
d(7) --> l(`seven`).
d(8) --> l(`eight`).
d(9) --> l(`nine`).
d(D) --> digit(C), {number_codes(D, [C])}.

char([D]) --> d(D), !.
char([]) --> nonblank(_).

line(Ds) --> char(C), sequence(char, Cs), eol, {append([C|Cs], Ds)}.

lines([L|Ls]) --> line(L), sequence(line, Ls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

digits_number(D1, D2, N) :-
    maplist(number_codes, [D1, D2, N], [[C0], [C1], [C0, C1]]).

line_number([F|Ds], N) :-
    (   length(Ds, 0)
    ->  F=L
    ;   last(Ds, L)
    ),
    digits_number(F, L, N).

solve(Sum) :-
    phrase_from_file(lines(Ls), 'input.txt'),
    maplist(line_number, Ls, Ns),
    sum_list(Ns, Sum).
