%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spring(S) --> operational(S) | broken(S) | unknown(S).
operational('.') --> `.` .
broken('#') --> `#` .
unknown('?') --> `?` .

springs([S|Ss]) --> spring(S), sequence(spring, Ss).

broken_groups([DG|DGs]) --> number(DG), `,`, broken_groups(DGs).
broken_groups([DG]) --> number(DG).

row(row(Ss, DGs)) --> springs(Ss), ` `, broken_groups(DGs), eol.

rows([R|Rs]) --> row(R), sequence(row, Rs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Grp = grp(GS, GE, GL, Bs_in).
% RL = RowLen
%
% GL = GroupLen
% GS = GroupStart
% GE = GroupEndGrou
%
% S = Spring
% B = Broken
% O = Operational

row_Bs_Os(row(Ss, _), Bs, Os) :-
    length(Ss, L),
    numlist(1, L, Idxs),
    pairs_keys_values(IdxSs, Idxs, Ss),
    include([Idx-'#']>>true, IdxSs, Bs0),
    include([Idx-'.']>>true, IdxSs, Os0),
    pairs_keys(Bs0, Bs),
    pairs_keys(Os0, Os).

grp(RL, Bs, Os, GL, grp(GS, GE, GL, Bs_in)) :-
    [GS, GE] ins 1..RL,
    GE #= GS + GL - 1,
    maplist({GS, GE}/[B, B_in]>>((GS #=< B #/\ B #=< GE) #<==> B_in), Bs, Bs_in),
    maplist({GS, GE}/[O]>>(O #< GS #\ O #> GE), Os).

grps_before([_]).
grps_before([Grp1,Grp2|Grps]) :-
    grps_before_(Grp1,Grp2),
    grps_before([Grp2|Grps]).

grps_before_(grp(GS1, _, GL1, _), grp(GS2, _, _, _)) :-
    GS1 + GL1 #< GS2.
    
brokens_in_group(Grps) :-
    maplist(arg(4), Grps, Bss0),
    transpose(Bss0, Bss),
    maplist([Bs]>>sum(Bs, #=, 1), Bss).

row_groups(Row, Grps) :-
    Row = row(Ss, GLs),
    length(Ss, RL),
    row_Bs_Os(Row, Bs, Os),
    maplist(grp(RL, Bs, Os), GLs, Grps),
    grps_before(Grps),
    brokens_in_group(Grps),
    maplist(arg(1), Grps, GSs),
    label(GSs).

nb_arrangments(Row, Nb) :-
    findall(Grps, row_groups(Row, Grps),  Grpss),
    length(Grpss, Nb).

solve1(S) :-
    phrase_from_file(rows(Rs), 'input.txt'),
    maplist(nb_arrangments, Rs, Nbs),
    sum_list(Nbs, S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve2(_S) :-
    phrase_from_file(_, 'small_input.txt').
