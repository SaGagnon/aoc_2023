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
    map_in_out(M, N0, N1),
    apply_maps(Ms, N1, N).

map_in_out(M, N0, N) :-
    M = map(_, Ranges),
    maplist(in_range(N0, N), InRanges, Ranges),
    foldl([P,Q,R]>>(P #\ Q #<==> R), InRanges, 0, InRange),
    InRange #\ N0 #= N.
    

in_range(N0, N, InRange, range(dest_range_start(DRS),
                               src_range_start(SRS),
                               range_len(RL)))
:-
    L0 #= SRS,
    U0 #= SRS + RL - 1,
    InRange #<==> N0 in L0..U0,
    RangeDiff #= N0 - L0,
    InRange #==> N #= DRS + RangeDiff.

solve1(X) :-
    phrase_from_file(almanac(Seeds, Maps), 'small_input.txt'),
    maplist(apply_maps(Maps), Seeds, Locations),
    min_list(Locations, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seedranges_domain(SRs, D, Div) :-
    seedranges_domains_(SRs, Ds, Div),
    SRs=[Start|_],
    foldl([X, Y, X\/Y]>>true, Ds, Start, D).

seedranges_domains_([], [], _).
seedranges_domains_([Start, Length|Rest], [D|Ds], Div) :-
    End #= Start + Length - 1,
    D = Start..End,
    seedranges_domains_(Rest, Ds, Div).

seed_generator(Mult, Seed) :-
    Seed #>= 0,
    Seed #=< 6000000000,
    Seed #= _ * Mult.

solve2(Seed, Location, Div) :-
    phrase_from_file(almanac(SRs, Maps), 'input.txt'),
    seedranges_domain(SRs, D, Div),
    Seed in D,
    Location in 0..2008895,
    apply_maps(Maps, Seed, Location).
    
% ?- findall(Seed-Location, (seed_generator(100000, Seed), indomain(Seed), solve2(Seed, Location, _)), SLs).
% SLs = [4076400000-2008895, 4076500000-2108895, 4076600000-2208895, 4076700000-2308895, 4076800000-2408895, 4076900000-2508895, 4077000000-2608895].

% ?- solve2(Seed, Location, _), Seed #= 4076400000-80838.
%false.

%?- solve2(Seed, Location, _), Seed #= 4076400000-80837.
%Seed = 4076319163,
%Location = 1928058 
