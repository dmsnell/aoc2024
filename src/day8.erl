-module(day8).
-moduledoc """
Started at 10:00pm
Part 1 solved at 10:30pm
Part 2 solved at 10:36pm
Solved in 36 minutes.

@author Dennis Snell <dmsnell@xkq.io>
@copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
Created : 07. Dec 2024 9:56 PM
""".
-author("Dennis Snell <dmsnell@xkq.io>").

%% API
-export([config/0]).

config() -> #{
    p1 => {fun p1_submitted/1, lines},
    p2 => {fun p2_submitted/1, lines},

    p2_optimized => {fun p2_only_as_far_as_need_be/1, lines}
}.


-doc """
#### Benchmark

```
#{total => {1306.567,ms},
  answer => 289,
  measured => {avg,{0.087,ms},min,{0.062,ms}},
  total_per => {0.131,ms}}
```
""".
p1_submitted([Line | _] = Lines) ->
    Size = byte_size(Line),
    Antenna = build_map(Lines, 1, #{}),
    AntinodeLocations = lists:foldl(
        fun ({_Freq, Locations}, Antinodes) ->
            Pairs = pairs(Locations),
            NewNodes = lists:foldl(
                fun ({{AX, AY}, {BX, BY}}, Locs) ->
                    DeltaX = BX - AX,
                    DeltaY = BY - AY,
                    A1 = {AX - DeltaX, AY - DeltaY},
                    A2 = {BX + DeltaX, BY + DeltaY},
                    As = [{A, true} || {X, Y} = A <- [A1, A2], X >= 1, X =< Size, Y >= 1, Y =< Size],
                    maps:merge(Locs, maps:from_list(As))
                end,
                #{},
                Pairs
            ),
            maps:merge(Antinodes, NewNodes)
        end,
        #{},
        maps:to_list(Antenna)
    ),
    maps:size(AntinodeLocations).

build_map([], _Row, Antenna) ->
    Antenna;

build_map([Line | Lines], Row, Antenna) ->
    NextAntenna = scan_line(Line, Row, Antenna),
    build_map(Lines, Row + 1, NextAntenna).

scan_line(Line, Row, Antenna) ->
    scan_line(Line, Row, 1, Antenna).

scan_line(<<>>, _Row, _Col, Antenna) ->
    Antenna;

scan_line(<<".", Rest/binary>>, Row, Col, Antenna) ->
    scan_line(Rest, Row, Col + 1, Antenna);

scan_line(<<Freq, Rest/binary>>, Row, Col, Antenna) ->
    scan_line(
        Rest,
        Row,
        Col + 1,
        maps:update_with(
            Freq,
            fun (L) -> [{Row, Col} | L] end,
            [{Row, Col}],
            Antenna
        )
    ).

pairs(List) ->
    lists:flatten(pairs(List, [])).

pairs([], Combinations) ->
    Combinations;

pairs([Head | Tail], Combinations) ->
    pairs(Tail, [[{Head, E} || E <- Tail] | Combinations]).


-doc """
Hasty attempt to finish the problem before bed. Assuming
the map is not too big, we can simply extend the antinode
search along each line out to multiples of 100.

#### Benchmark

```
#{total => {16761.443,ms},
  answer => 1030,
  measured => {avg,{1.624,ms},min,{1.264,ms}},
  total_per => {1.676,ms}}
```
""".
p2_submitted([Line | _] = Lines) ->
    Size = byte_size(Line),
    Antenna = build_map(Lines, 1, #{}),
    AntinodeLocations = lists:foldl(
        fun ({_Freq, Locations}, Antinodes) ->
            Pairs = pairs(Locations),
            NewNodes = lists:foldl(
                fun ({{AX, AY}, {BX, BY}}, Locs) ->
                    DeltaX = BX - AX,
                    DeltaY = BY - AY,
                    NNewLocs = lists:foldl(
                        fun (N, NLocs) ->
                            A1 = {AX - N * DeltaX, AY - N * DeltaY},
                            A2 = {BX + N * DeltaX, BY + N * DeltaY},
                            A3 = {BX - N * DeltaX, BY - N * DeltaY},
                            A4 = {AX + N * DeltaX, AY + N * DeltaY},
                            As = [{A, true} || {X, Y} = A <- [A1, A2, A3, A4], X >= 1, X =< Size, Y >= 1, Y =< Size],
                            maps:merge(NLocs, maps:from_list(As))
                        end,
                        #{},
                        lists:seq(1,100)
                    ),
                    maps:merge(Locs, NNewLocs)
                end,
                #{},
                Pairs
            ),
            maps:merge(Antinodes, NewNodes)
        end,
        #{},
        maps:to_list(Antenna)
    ),
    maps:size(AntinodeLocations).


-doc """
In order to finish quickly, in the submitted part 2, I figured
that going out _100 distances_ would be sufficient for finding
all of the antinodes, and this was right, but inefficient.

This is the version that should have been submitted first, but
didn’t because I was tired.

#### Benchmark

```
#{total => {5638.769,ms},
  answer => 1030,
  measured => {avg,{0.506,ms},min,{0.406,ms}},
  total_per => {0.564,ms}}
```
""".
p2_only_as_far_as_need_be([Line | _] = Lines) ->
    Size = byte_size(Line),
    Antenna = build_map(Lines, 1, #{}),
    AntinodeLocations = lists:foldl(
        fun ({_Freq, Locations}, Antinodes) ->
            Pairs = pairs(Locations),
            NewNodes = lists:foldl(
                fun ({{AX, AY} = A, {BX, BY} = B}, Locs) ->
                    DeltaX = BX - AX,
                    DeltaY = BY - AY,
                    maps:merge(
                        Locs,
                        maps:from_list(lists:flatten([
                            hunt(Size, {DeltaX, DeltaY}, A),
                            hunt(Size, {-DeltaX, -DeltaY}, A),
                            hunt(Size, {DeltaX, DeltaY}, B),
                            hunt(Size, {-DeltaX, -DeltaY}, B)
                        ]))
                    )
                end,
                #{},
                Pairs
            ),
            maps:merge(Antinodes, NewNodes)
        end,
        #{},
        maps:to_list(Antenna)
    ),
    maps:size(AntinodeLocations).


hunt(Size, Delta, From) ->
    hunt(Size, Delta, day6:step(Delta, From), []).

hunt(Size, _Delta, {X, Y} = _From, Antinodes) when X < 1; Y < 1; X > Size; Y > Size ->
    Antinodes;

hunt(Size, Delta, From, Antinodes) ->
    hunt(Size, Delta, day6:step(Delta, From), [{From, true} | Antinodes]).


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


p1_test() ->
    ?assertEqual(14, aoc:test(day8, p1, "day8_a")).

p1_answer_test() ->
    ?assertEqual(289, aoc:test(day8, p1, "day8")).

p2_test() ->
    ?assertEqual(34, aoc:test(day8, p2, "day8_a")).

p2_answer_test_() -> [
    {"Submitted", ?_assertEqual(1030, aoc:test(day8, p2, "day8"))},
    {"Only-as-far", ?_assertEqual(1030, aoc:test(day8, p2_optimized, "day8"))}
].

-endif.
