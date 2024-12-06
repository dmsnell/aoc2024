-module(day6).
-moduledoc """
Started at 10:00pm
Part 1 solved at 11:00pm [first attempt, 313, was not successful (min/max copy mistake)]
Part 2 solved at…
Solved in…

@author Dennis Snell <dmsnell@xkq.io>
@copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
Created : 05. Dec 2024 8:21 PM
""".
-author("Dennis Snell <dmsnell@xkq.io>").

%% API
-export([config/0]).

config() -> #{
    p1 => {fun p1_submitted/1, lines},
    p2 => {fun p2_submitted/1, lines}
}.


p1_submitted(Lines) ->
    {ok, {_Rows, _Cols} = Size, Obstacles, Guard} = build_map(Lines),
    {_Exit, Traversed} = navigate(Size, Obstacles, Guard),
    maps:size(Traversed).


build_map(Lines) ->
    build_map({0, 0}, #{}, no_guard, 1, Lines).

build_map(Size, Obstacles, {_, _, up} = Guard, _Row, []) ->
    {ok, Size, Obstacles, Guard};

build_map({H, _W}, Obstacles, Guard, Row, [Line | Lines]) ->
    {NewObstacles, NewGuard} = parse_line(Obstacles, Guard, Row, 1, Line),
    build_map({H + 1, byte_size(Line)}, NewObstacles, NewGuard, Row + 1, Lines).


parse_line(Obstacles, Guard, _Row, _Col, <<>>) ->
    {Obstacles, Guard};
parse_line(Obstacles, Guard, Row, Col, <<"#", Line/binary>>) ->
    parse_line(Obstacles#{{Row, Col} => true}, Guard, Row, Col + 1, Line);
parse_line(Obstacles, no_guard, Row, Col, <<"^", Line/binary>>) ->
    parse_line(Obstacles, {Row, Col, up}, Row, Col + 1, Line);
parse_line(Obstacles, Guard, Row, Col, <<_, Line/binary>>) ->
    parse_line(Obstacles, Guard, Row, Col + 1, Line).


navigate({_H, _W} = Size, Obstacles, {Row, Col, _Dir} = Guard) ->
    navigate(Size, Obstacles, Guard, log_new({Row, Col}, up)).

navigate({H, W} = Size, Obstacles, {Row, Col, Dir} = _Guard, Traversed) ->
    From = {Row, Col},
    case Dir of
        up ->
            case [R || {R, C} := _ <- Obstacles, C == Col, R < Row] of
                [] ->
                    {{1, Col}, traverse(Traversed, Dir, From, {1, Col})};
                InWay ->
                    R = lists:max(InWay),
                    navigate(
                        Size,
                        Obstacles,
                        {R + 1, Col, turn(Dir)},
                        traverse(Traversed, Dir, From, {R + 1, Col})
                    )
            end;

        down ->
            case [R || {R, C} := _ <- Obstacles, C == Col, R > Row] of
                [] ->
                    {{H, Col}, traverse(Traversed, Dir, From, {H, Col})};
                InWay ->
                    R = lists:min(InWay),
                    navigate(
                        Size,
                        Obstacles,
                        {R - 1, Col, turn(Dir)},
                        traverse(Traversed, Dir, From, {R - 1, Col})
                    )
            end;

        left ->
            case [C || {R, C} := _ <- Obstacles, R == Row, C < Col] of
                [] ->
                    {{Row, 1}, traverse(Traversed, Dir, From, {Row, 1})};
                InWay ->
                    C = lists:max(InWay),
                    navigate(
                        Size,
                        Obstacles,
                        {Row, C + 1, turn(Dir)},
                        traverse(Traversed, Dir, From, {Row, C + 1})
                    )
            end;

        right ->
            case [C || {R, C} := _ <- Obstacles, R == Row, C > Col] of
                [] ->
                    {{Row, W}, traverse(Traversed, Dir, From, {Row, W})};
                InWay ->
                    C = lists:min(InWay),
                    navigate(
                        Size,
                        Obstacles,
                        {Row, C - 1, turn(Dir)},
                        traverse(Traversed, Dir, From, {Row, C - 1})
                    )
            end
    end.


traverse(Seen, Dir, {FR, FC} = From, {TR, TC} = To) ->
    Delta = {delta(FR, TR), delta(FC, TC)},
    traverse(
        log(Seen, From, Dir),
        Dir,
        Delta,
        step(Delta, From),
        To
    ).

traverse(Seen, Dir, _Delta, To, To) ->
    log(Seen, To, Dir);
traverse(Seen, Dir, Delta, From, To) ->
    traverse(log(Seen, From, Dir), Dir, Delta, step(Delta, From), To).

log_new({R, C} = _At, Dir) ->
    #{{R, C} => #{Dir => true}}.

log(Seen, At, Dir) ->
    maps:update_with(
        At,
        fun (Dirs) -> Dirs#{Dir => true} end,
        #{Dir => true},
        Seen
    ).


delta(up) -> {-1, 0};
delta(right) -> {0, 1};
delta(down) -> {1, 0};
delta(left) -> {0, -1}.

delta(From, From) -> 0;
delta(From, To) when From < To -> 1;
delta(From, To) when From > To -> -1.

step({DR, DC} = _Delta, {R, C} = _From) ->
    {R + DR, C + DC}.


turn(up) -> right;
turn(right) -> down;
turn(down) -> left;
turn(left) -> up.


p2_submitted(Lines) ->
    {ok, {_Rows, _Cols} = Size, Obstacles, {Row, Col, _} = Guard} = build_map(Lines),
    {_Exit, #{loops := Loops}} = navigate(Size, Obstacles, Guard),
    Blockers = maps:remove({Row, Col}, Loops),
    Blockers.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(41, aoc:test(day6, p1, "day6_a")).

p2_test() ->
    ?assertEqual(6, aoc:test(day6, p2, "day6_a")).

-endif.
