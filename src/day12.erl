-module(day12).
-moduledoc """
Started at 10:00pm
Part 1 solved at 12:49am
Part 2 solved at 1:45am
Solved in 3h 45m

@author Dennis Snell <dmsnell@xkq.io>
@copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
Created : 11. Dec 2024 9:53 PM
""".
-author("Dennis Snell <dmsnell@xkq.io>").

%% API
-export([config/0]).

config() -> #{
    p1 => {fun p1_submitted/1, lines},
    p2 => {fun p2_submitted/1, lines}
}.


-doc """
#### Benchmark

```
{day12,p1,
   #{total => {84380.436,ms},
     answer => 1485656,
     measured => {avg,{84.256,ms},min,{78.344,ms}},
     total_per => {84.38,ms}}}
```
""".
p1_submitted([First | _] = Lines) ->
%%    io:format("~n"),
    Size = byte_size(First),
    Map  = build_map(Size, Lines),
%%    io:format("Map~n"),
%%    zone_map(Size, Map),
    Merged  = merge_zones(Size, Map),
%%    io:format("~nMerged Map~n"),
%%    zone_map(Size, Merged),
    Counts = count(Merged),
    Count = maps:fold(
        fun (_Zone, {_Plot, Area, Perimeter}, Sum) ->
            Sum + Area * Perimeter
        end,
        0,
        Counts
    ),
%%    {Merged, Counts, Count}.
    Count.


build_map(Size, Lines) ->
    Boundary = list_to_binary(lists:duplicate(Size, 0)),
    Map = map_row(Boundary, [Boundary | Lines], 1, #{}),
    maps:filter(
        fun
            ({R, C}, _V) when R >= 1, R =< Size, C >= 1, C =< Size ->
                true;

            (_, _) ->
                false
        end,
        Map
    ).

map_row(Boundary, [L1, L2], Row, Map) ->
    map_cols({<<0, L1/binary, 0>>, <<0, L2/binary, 0>>, <<0, Boundary/binary, 0>>}, Row, 1, Map);

map_row(Boundary, [L1, L2, L3 | Lines], Row, Map) ->
    NextMap = map_cols({<<0, L1/binary, 0>>, <<0, L2/binary, 0>>, <<0, L3/binary, 0>>}, Row, 1, Map),
    map_row(Boundary, [L2, L3 | Lines], Row + 1, NextMap).


map_cols({
    <<_L1a, L1b, _L1c>>,
    << L2a, L2b,  L2c>>,
    <<_L3a, L3b, _L3c>>
}, Row, Col, Map) ->
    Neighbors = [
        {{Row - 1, Col}, L1b},
        {{Row, Col - 1}, L2a},
        {{Row, Col + 1}, L2c},
        {{Row + 1, Col}, L3b}
    ],
%%    io:format("\e[90m(\e[2;31mr\e[0;90m, \e[2;31mc\e[0;90m) => (\e[33m~p\e[90m, \e[33m~p\e[90m) = \e[34m~ts\e[m~n", [Row, Col, [L2b]]),
    {Zone, ZoneMap} = get_zone(L2b, Neighbors, Map),
    Perimeter = lists:sum([1 || Plot <- [L1b, L2a, L2c, L3b], Plot =/= L2b]),
    maps:put({Row, Col}, {L2b, Zone, Perimeter}, ZoneMap);

map_cols({
    <<_L1a, L1b, L1c, L1/binary>>,
    << L2a, L2b, L2c, L2/binary>>,
    <<_L3a, L3b, L3c, L3/binary>>
}, Row, Col, Map) ->
    Neighbors = [
        {{Row - 1, Col}, L1b},
        {{Row, Col - 1}, L2a},
        {{Row, Col + 1}, L2c},
        {{Row + 1, Col}, L3b}
    ],
%%    io:format("\e[90m(\e[2;31mr\e[0;90m, \e[2;31mc\e[0;90m) => (\e[33m~p\e[90m, \e[33m~p\e[90m) = \e[34m~ts\e[m~n", [Row, Col, [L2b]]),
    {Zone, ZoneMap} = get_zone(L2b, Neighbors, Map),
    Perimeter = lists:sum([1 || Plot <- [L1b, L2a, L2c, L3b], Plot =/= L2b]),
    NextMap = maps:put({Row, Col}, {L2b, Zone, Perimeter}, ZoneMap),
    map_cols({
        <<L1b, L1c, L1/binary>>,
        <<L2b, L2c, L2/binary>>,
        <<L3b, L3c, L3/binary>>
    }, Row, Col + 1, NextMap).


get_zone(_For, [], Map) ->
    {Zone, Next} = get_next_zone(Map),
%%    io:format("\e[90m  assigning to zone \e[33m~p\e[m\n", [Zone]),
    {Zone, Next};
get_zone(For, [{P, For} | Neighbors], Map) ->
%%    io:format("\e[90m  is \e[2;33m~p\e[0;90m already in a zone?~n", [P]),
    case maps:find(P, Map) of
        {ok, {For, Zone, _Perimeter}} ->
%%            io:format("\e[32m  yes\e[90m in the \e[35m~p\e[90m zone\e[m\n", [Zone]),
            {Zone, Map};

        {ok, {_Plot, _, _}} ->
%%            io:format("\e[32m  yes\e[90m but for plot type \e[35m~ts\e[m\n", [[_Plot]]),
            get_zone(For, Neighbors, Map);

        error ->
%%            io:format("\e[31m  no\e[m\n"),
            get_zone(For, Neighbors, Map)
    end;
get_zone(For, [_ | Neighbors], Map) ->
    get_zone(For, Neighbors, Map).


get_next_zone(Map) ->
    NextMap = maps:update_with(
        zone_counter,
        fun (Z) -> Z + 1 end,
        1,
        Map
    ),
    {maps:get(zone_counter, NextMap), NextMap}.


color(N) ->
    case N rem 8 of
        0 -> "\e[30m";
        1 -> "\e[31m";
        2 -> "\e[32m";
        3 -> "\e[33m";
        4 -> "\e[34m";
        5 -> "\e[35m";
        6 -> "\e[36m";
        7 -> "\e[37m"
    end.


zone_map(Size, Map) ->
    Lines = [
        lists:join("", [
            begin
                {P, Z, _} = maps:get({R, C}, Map),
                io_lib:format("~ts~ts\e[m", [color(Z * 33 rem 23 * 7), [P]])
            end
            ||
            C <- lists:seq(1, Size)
        ])
        ||
        R <- lists:seq(1, Size)
    ],
    lists:map(fun (L) -> io:format("~ts~n", [L]) end, Lines).


merge_zones(Size, Map) ->
    NextMap = maps:fold(
        fun ({R, C}, {Plot, Zone, Perimeter}, PrevMap) ->
            Neighbors = [
                maps:get(N, PrevMap)
                ||
                {NR, NC} = N <- [{R - 1, C}, {R, C - 1}, {R, C + 1}, {R + 1, C}],
                NR >= 1, NR =< Size, NC >= 1, NC =< Size
            ],
            case [NZ || {NPlot, NZ, _NP} <- Neighbors, NPlot == Plot, NZ < Zone] of
                [] ->
                    PrevMap;

                LowerZones ->
%%                    io:format("\e[90mMerging zone \e[31m~p\e[90m into \e[32m~p\e[m\n", [Zone, lists:min(LowerZones)]),
                    maps:update({R, C}, {Plot, lists:min(LowerZones), Perimeter}, PrevMap)
            end
        end,
        Map,
        Map
    ),
    % Keep merging groups until there are no more merges.
    case NextMap of
        Map ->
            Map;

        _ ->
            merge_zones(Size, NextMap)
    end.



count(Map) ->
    count(maps:next(maps:iterator(Map)), #{}).

count(none, Counts) ->
    Counts;
count({_P, {Plot, Zone, Perimeter}, Map}, Counts) ->
    NextCounts = maps:update_with(
        Zone,
        fun ({PT, C, P}) -> {PT, C + 1, P + Perimeter} end,
        {Plot, 1, Perimeter},
        Counts
    ),
    count(maps:next(Map), NextCounts).


-doc """
#### Benchmark

```
{day12,p2,
   #{total => {102738.029,ms},
     answer => 899196,
     measured => {avg,{102.598,ms},min,{92.104,ms}},
     total_per => {102.738,ms}}}
```
""".
p2_submitted([First | _] = Lines) ->
    Size = byte_size(First),
    Map  = build_map(Size, Lines),
    Merged  = merge_zones(Size, Map),
%%    io:format("~nMerged Map~n"),
%%    zone_map(Size, Merged),
    Edges = edge_sweep(Size, Merged),
    EdgeMap = merge_edges(Edges),
    maps:fold(
        fun (_Z, {Area, EdgeCount}, Sum) ->
            Sum + Area * EdgeCount
        end,
        0,
        EdgeMap
    ).


edge_sweep(Size, Map) ->
    maps:fold(
        fun ({R, C}, {_T, Z, _P}, Prev) ->
            Neighbors = [
                begin
                    {_, NZ, _} = maps:get({NR, NC}, Map),
                    {{D, true}, NZ}
                end
                ||
                {D, {NR, NC}} <- [
                    {top, {R - 1, C}},
                    {right, {R, C + 1}},
                    {left, {R, C - 1}},
                    {bottom, {R + 1, C}}
                ],
                NR >= 1, NR =< Size, NC >= 1, NC =< Size
            ],
            NeighborEdges = [D || {D, NZ} <- Neighbors, NZ =/= Z],
            MapEdges = lists:flatten([
                [{D, true} || D <- [top], R == 1],
                [{D, true} || D <- [right], C == Size],
                [{D, true} || D <- [bottom], R == Size],
                [{D, true} || D <- [left], C == 1]
            ]),
            Edges = lists:uniq(NeighborEdges ++ MapEdges),
            maps:update({R, C}, {Z, maps:from_list(Edges)}, Prev)
        end,
        Map,
        Map
    ).

merge_edges(Map) ->
    EdgeMap = maps:fold(
        fun ({R, C}, {Zone, Edges}, Prev) ->
            Top = follows_edge(Map, Zone, Edges, {R, C - 1}, top),
            Right = follows_edge(Map, Zone, Edges, {R - 1, C}, right),
            Bottom = follows_edge(Map, Zone, Edges, {R, C - 1}, bottom),
            Left = follows_edge(Map, Zone, Edges, {R - 1, C}, left),
            NewEdges = Top + Right + Bottom + Left,
%%            io:format(
%%                "{R C}@{~p ~p} has ~p new edges: [top right bottom left]@[~p ~p ~p ~p]~n",
%%                [R, C, NewEdges, Top, Right, Bottom, Left]
%%            ),
            maps:update_with(
                Zone,
                fun ({Area, Sum}) -> {Area + 1, Sum + NewEdges} end,
                {1, NewEdges},
                Prev
            )
        end,
        #{},
        Map
    ),
    EdgeMap.

follows_edge(Map, Zone, Edges, {R, C}, Dir) ->
    case {Edges, maps:find({R, C}, Map)} of
            {#{Dir := _}, {ok, {Zone, #{Dir := _}}}} -> 0;
            {#{Dir := _}, {ok, {_, #{}}}} -> 1;
            {#{Dir := _}, error} -> 1;
            _ -> 0
    end.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test_() -> [
    {"Example 1", ?_assertEqual(140, aoc:test(day12, p1, "day12_a"))},
    {"Example 2", ?_assertEqual(772, aoc:test(day12, p1, "day12_b"))},
    {"Example 3", ?_assertEqual(1930, aoc:test(day12, p1, "day12_c"))},
    {"Solution", ?_assertEqual(1485656, aoc:test(day12, p1, "day12"))}
].

p2_test_() -> [
    {"Example 1", ?_assertEqual(80, aoc:test(day12, p2, "day12_a"))},
    {"Example 2", ?_assertEqual(436, aoc:test(day12, p2, "day12_b"))},
    {"Example 3", ?_assertEqual(1206, aoc:test(day12, p2, "day12_c"))},
    {"Example 4", ?_assertEqual(236, aoc:test(day12, p2, "day12_d"))},
    {"Example 5", ?_assertEqual(368, aoc:test(day12, p2, "day12_e"))},
    {"Solution", ?_assertEqual(899196, aoc:test(day12, p2, "day12"))}
].

-endif.
