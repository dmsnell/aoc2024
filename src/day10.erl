-module(day10).
-moduledoc """
Started at 10:00pm
Paused at 10:10pm (too tired to work on this)
Started again at 11:40am
Paused at 12:00pm, back at 12:30pm (easier to work at night than when people are awake).
Part 1 solved at 12:35pm
Part 2 solved at 1:01pm
Solved in 1h 1m.

@author Dennis Snell <dmsnell@xkq.io>
@copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
Created : 09. Dec 2024 9:54 PM
""".
-author("Dennis Snell <dmsnell@xkq.io>").

%% API
-export([config/0]).

config() -> #{
    p1 => {fun p1_submitted/1, lines},
    p2 => {fun p2_submitted/1, lines}
}.

-doc """
Submitted solution.

#### Benchmark

```
{day10,p1,
   #{total => {16040.37,ms},
     answer => 510,
     measured => {avg,{1.56,ms},min,{1.317,ms}},
     total_per => {1.604,ms}}}
```
""".
p1_submitted(Lines) ->
    {ok, Map, Size} = build_map(Lines),
    Trailheads = maps:keys(maps:filter(fun (_K, V) -> V == 0 end, Map)),
    lists:foldl(
        fun (Head, Sum) ->
            {_Visited, Ends} = trails(Head, Map, Size),
            Sum + maps:size(Ends)
        end,
        0,
        Trailheads
    ).


build_map([First | _] = Lines) ->
    Size = byte_size(First),
    build_map(Lines, Size, 1, #{}).

build_map([], Size, _Row, Map) ->
    {ok, Map, Size};

build_map([Line | Lines], Size, Row, Map) ->
    build_map(Lines, Size, Row + 1, parse_line(Line, Row, Map)).

parse_line(Line, Row, Map) ->
    parse_line(Line, Row, 1, Map).

parse_line(<<>>, _Row, _Col, Map) ->
    Map;

parse_line(<<D, Buffer/binary>>, Row, Col, Map) when D >= $0, D =< $9 ->
    parse_line(Buffer, Row, Col + 1, Map#{{Row, Col} => D - $0}).

trails(Head, Map, Size) ->
    trails(Head, 0, Map, Size, #{}, #{}).

trails(_From, 9, _Map, _Size, Visited, Ends) ->
    {Visited, Ends};

trails(From, PrevHeight, Map, Size, Visited, Ends) ->
    Steps = get_steps(Map, Size, From, PrevHeight, Visited),
    lists:foldl(
        fun ({P, H}, {PrevSteps, PrevEnds}) ->
            {NextSteps, NextEnds} = trails(P, H, Map, Size, PrevSteps, PrevEnds),
            {NextSteps, maps:merge(PrevEnds, NextEnds)}
        end,
        {Visited, maps:merge(Ends, maps:from_list([{P, true} || {P, 9} <- Steps, not is_map_key(P, Ends)]))},
        Steps
    ).

get_steps(Map, Size, {R, C} = _From, PrevHeight, Visited) ->
    Neighbors = [
        {{R + DR, C + DC}, maps:get({R + DR, C + DC}, Map)}
        ||
        {DR, DC} <- [{-1, 0}, {0, 1}, {1, 0}, {0, -1}],
        R + DR > 0, R + DR =< Size,
        C + DC > 0, C + DC =< Size,
        not is_map_key({R + DR, C + DC}, Visited)
    ],
    [{P, H} || {P, H} <- Neighbors, H == PrevHeight + 1].


-doc """
Submitted solution.

#### Benchmark

```
{day10,p2,
   #{total => {21178.107,ms},
     answer => 1058,
     measured => {avg,{2.069,ms},min,{1.783,ms}},
     total_per => {2.118,ms}}}
```
""".
p2_submitted(Lines) ->
    {ok, Map, Size} = build_map(Lines),
    Trailheads = maps:keys(maps:filter(fun (_K, V) -> V == 0 end, Map)),
    lists:foldl(fun (Head, Sum) -> Sum + trails2(Head, Map, Size) end, 0, Trailheads).


trails2(Head, Map, Size) ->
    trails2(Head, 0, Map, Size, #{}).

trails2(From, PrevHeight, Map, Size, Visited) ->
    case get_steps2(Map, Size, From, PrevHeight, Visited) of
        % No further steps means this is a dead-end path.
        [] -> 0;

        % Multiple steps means multiple potential paths.
        % Take each one and sum the results, one at a time
        % to avoid the combinatorial explosion. But separating
        % the visited history.
        Steps = [_|_] ->
            lists:foldl(
                fun
                    % Found a trail end, this path was successful.
                    ({_Step, 9}, Count) ->
                        Count + 1;

                    ({Step, H}, Count) ->
                        Count + trails2(Step, H, Map, Size, maps:put(From, true, Visited))
                end,
                0,
                Steps
            )
    end.



bump_ends(PrevEnds, Endings) ->
    maps:fold(
        fun (End, _Count, Ends) -> maps:update_with(End, fun (C) -> C + 1 end, 1, Ends) end,
        PrevEnds,
        Endings
    ).

get_steps2(Map, Size, {R, C} = _From, PrevHeight, Visited) ->
    Neighbors = [
        {{R + DR, C + DC}, maps:get({R + DR, C + DC}, Map)}
        ||
        {DR, DC} <- [{-1, 0}, {0, 1}, {1, 0}, {0, -1}],
        R + DR > 0, R + DR =< Size,
        C + DC > 0, C + DC =< Size,
        not is_map_key({R + DR, C + DC}, Visited)
    ],
    [{P, H} || {P, H} <- Neighbors, H == PrevHeight + 1].


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(36, aoc:test(day10, p1, "day10_a")).

p2_test() ->
    ?assertEqual(81, aoc:test(day10, p2, "day10_a")).

-endif.
