%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

line(Cs) --> nonblank(C_), nonblanks(Cs_), eol,
             { maplist(char_code, Cs, [C_|Cs_]) }.

lines([L|Ls]) --> line(L), sequence(line, Ls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Matrix

lines_mat(Ls, M) :-
    maplist([L, I]>>(I=..[i|L]), Ls, Is),
    M=..[j|Is].

mat_ij(M, c(I,J), Pipe) :-
    arg(J0, M, Is),
    arg(I0, Is, Pipe),
    I #= I0 - 1, J #= J0 - 1.

%% Neighborhood

north(c(I,J0), c(I,J)) :- J #= J0 - 1.
south(c(I,J0), c(I,J)) :- J #= J0 + 1.
west(c(I0,J), c(I,J))  :- I #= I0 - 1.
east(c(I0,J), c(I,J))  :- I #= I0 + 1.

nei_('S', M, C0, C) :- north(C0, C), mat_ij(M, C, P), member(P, ['|','7','F']).
nei_('S', M, C0, C) :- east(C0, C),  mat_ij(M, C, P), member(P, ['-','J','7']).
nei_('S', M, C0, C) :- south(C0, C), mat_ij(M, C, P), member(P, ['|','L','J']).
nei_('S', M, C0, C) :- west(C0, C),  mat_ij(M, C, P), member(P, ['-','L','F']).
                       
nei_('|', _, C0, C) :- north(C0, C) ; south(C0, C).
nei_('-', _, C0, C) :-  east(C0, C) ;  west(C0, C).
nei_('L', _, C0, C) :- north(C0, C) ;  east(C0, C).
nei_('J', _, C0, C) :- north(C0, C) ;  west(C0, C).
nei_('7', _, C0, C) :- south(C0, C) ;  west(C0, C).
nei_('F', _, C0, C) :- south(C0, C) ;  east(C0, C).

nei(M, C0, C) :-
    mat_ij(M, C0, Pipe),
    nei_(Pipe, M, C0, C).

pipe(M, C) :- mat_ij(M, C, Pipe), dif(Pipe, '.').

%% Paths from S

path_from_s(M, Path) :-
    mat_ij(M, S, 'S'),
    dfs(M, [S], Path).

dfs(_, [C|Cs], C-L) :-
    length([C|Cs], L).
dfs(M, [C|Cs], CL) :-
    nei(M, C, CN),
    \+ member(CN, [C|Cs]),
    pipe(M, CN),
    dfs(M, [CN,C|Cs], CL).

mat_farthest_path(M, F) :-
    findall(N-L, path_from_s(M, N-L), NL0s),
    keysort(NL0s, NLs),
    group_pairs_by_key(NLs, GroupedNLs),
    pairs_values(GroupedNLs, L0s),
    maplist(min_list, L0s, Ls),
    max_list(Ls, F0),
    F #= F0 - 1.

solve1(X) :-
    phrase_from_file(lines(Ls), 'input.txt'),
    lines_mat(Ls, M),
    mat_farthest_path(M, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve2(_) :-
    phrase_from_file(_, 'small_input.txt').
