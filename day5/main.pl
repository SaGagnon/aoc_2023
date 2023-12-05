%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seeds(Ss) --> `seeds: `, seeds_(Ss), eol, eol.

seeds_([S|Ss]) --> number(S), blank, seeds_(Ss).
seeds_([S]) --> number(S).

range(range(
          dest_range_start(DRS),
          src_range_start(SRS),
          range_len(RL)))
-->
    number(DRS), blank, number(SRS), blank, number(RL), eol.

ranges([R|Rs]) --> range(R), ranges(Rs).
ranges([R]) --> range(R).

map(map(Name, Ranges)) --> nonblanks(Name_), ` map:`, eol, ranges(Ranges), eol,
                           { atom_codes(Name, Name_) }.

maps([M|Ms]) --> map(M), maps(Ms).
maps([M]) --> map(M).

almanac(Seeds, Maps) -->
    seeds(Seeds),
    maps(Maps).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apply_maps([], N, N).
apply_maps([M|Ms], N0, N) :-
    apply_map(M, N0, N1),
    apply_maps(Ms, N1, N).

apply_map(M, N0, N) :-
    M = map(_, Ranges),
    (   include(valid_range(N0), Ranges, [range(dest_range_start(DRS),
                                                src_range_start(SRS),
                                                range_len(_))|Rs])
    ->  (   Rs = []
        ->  RangePos #= N0 - SRS,
            N #= DRS + RangePos
        ;   throw('Two ranges matching')
        )
    ;   N0 = N
    ).

valid_range(N, range(dest_range_start(_),
                     src_range_start(SRS),
                     range_len(RL)))
:-
    N #>= SRS,
    N #< SRS + RL.

solve1(X) :-
    phrase_from_file(almanac(Seeds, Maps), 'input.txt'),
    maplist(apply_maps(Maps), Seeds, Locations),
    min_list(Locations, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seedrange_seeds(_, 0, []).
seedrange_seeds(Start0, Nb0, [Start|Ss]) :-
    Start #= Start0 + 1,
    Nb #= Nb0 - 1,
    seedrange_seeds(Start, Nb, Ss).

seedranges_bags([], []).
seedranges_bags([Start, Nb|Ranges], [S|Ss]) :-
    seedrange_seeds(Start, Nb, S),
    seedranges_bags(Ranges, Ss).

solve2(X) :-
    phrase_from_file(almanac(SRs, Maps), 'input.txt'),
    seedranges_bags(SRs, Bags),
    append(Bags, Seeds),
    maplist(apply_maps(Maps), Seeds, Locations),
    min_list(Locations, X).
    
