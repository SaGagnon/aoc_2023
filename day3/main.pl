%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DCG state:
%    s(I, J, Functors)

% Functors:
%    n(I, J, N)
%    sym(I, J, S)

num(s(I0, J, Fs), s(I, J, [n(I0,J,N)|Fs])) -->
    digits(Cs), 
    { length(Cs, L),
      L #> 0,
      number_codes(N, Cs),
      I #= I0 + L }.

dot(s(I0, J, Fs), s(I, J, Fs)) --> `.`, { I #= I0 + 1 }.

symbol(s(I0, J, Fs), s(I, J, [sym(I0,J,[S])|Fs])) -->
    [S],
    { maplist(dif([S]), [`.`, `\n`]),
      \+ code_type(S, digit),
      I #= I0 + 1 }.

token(S0, S) --> num(S0, S) | dot(S0, S) | symbol(S0, S).

tokens(S0, S) --> token(S0, S1), tokens(S1, S).
tokens(S0, S) --> token(S0, S).

line(S0, S) -->
    tokens(S0, S1), eol,
    { S1 =s(_, J0, Fs),
      S  =s(0, J,  Fs),
      J #= J0 + 1 }.

lines(S0, S) --> line(S0, S1), lines(S1, S).
lines(S0, S) --> line(S0, S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve1(S) :-
    once(
        phrase_from_file(lines(s(0, 0, []),
                               s(_, _, Fs)),
                         'input.txt')),
    maplist(assertz, Fs, Refs),
    findall(N, valid_number(N), Ns),
    sum_list(Ns, S),
    maplist(erase, Refs).

valid_number(N) :-
    n(I, J, N),
    number_codes(N, Cs),
    length(Cs, L),
    IMin #= I-1, IMax #= I+L,
    JMin #= J-1, JMax #= J+1,
    ISym in IMin..IMax,
    JSym in JMin..JMax,
    sym(ISym, JSym, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gear_or_num(n(_,_,_)).
gear_or_num(sym(_,_,`*`)).

n_end(I, J, N) :-
    n(I0, J, N),
    number_codes(N, Cs),
    length(Cs, L),
    I #= I0 + L - 1.

gear(Ratio) :-
    sym(I, J, `*`),
    IMin #= I-1, IMax #= I+1,
    JMin #= J-1, JMax #= J+1,
    IN in IMin..IMax,
    JN in JMin..JMax,
    findall(N, n(IN, JN, N), N0s),
    findall(N, n_end(IN, JN, N), N1s),
    append(N0s, N1s, N2s),
    sort(N2s, [N0,N1]),
    Ratio #= N0 * N1.


solve2(S) :-
    once(
        phrase_from_file(lines(s(0, 0, []),
                               s(_, _, Fs0)),
                         'input.txt')),
    include(gear_or_num, Fs0, Fs),
    maplist(assertz, Fs, Refs),
    findall(R, gear(R), Rs),
    sum_list(Rs, S),
    abolish(n/3),
    abolish(sym/3).
