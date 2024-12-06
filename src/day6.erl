-module(day6).
-moduledoc """
Started at 10:00pm
Part 1 solved at 11:00pm
    - first attempt, 313, was not successful (min/max copy mistake).
Part 2 solved at…
    - first attempt, 756, was not successful (pretty sure it would be wrong)
    - second, 714, was wrong (thought it was because I had previously added
      blockers where the path had already traversed, but this wasn’t the fix).
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
    maps:size(Traversed) - 3.


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
    navigate(Size, Obstacles, Guard, log_new(Obstacles, Size, {Row, Col}, up)).

navigate({H, W} = Size, Obstacles, {Row, Col, Dir} = _Guard, Traversed) ->
    From = {Row, Col},
    case Traversed of
        #{{Row, Col} := #{Dir := true}} ->
            loop;

        _ ->
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

log_new(Obstacles, Size, {_R, _C} = _At, _Dir) ->
    #{
%%        {R, C} => #{Dir => true},
        size => Size,
        loops => #{},
        obstacles => Obstacles
    }.

log(Seen, At, Dir) ->
    maps:update_with(
        At,
        fun (Dirs) -> Dirs#{Dir => true} end,
        #{Dir => true},
        find_blocker(Seen, At, Dir)
    ).

find_blocker(#{loops := Loops, size := Size, obstacles := Obstacles} = Seen, At, Dir) ->
    NeedDir = turn(Dir),
    case is_map_key(step(NeedDir, At), Obstacles) of
        true ->
            Seen;

        false ->
            case find_first_path(Obstacles, Seen, Size, NeedDir, At, At, #{}) of
                {ok, _Looper} ->
                    BlockerAt = step(Dir, At),
                    % Can’t install an obstacle where the path has already crossed.
                    case maps:get(BlockerAt, Seen, free) of
                        free ->
                            Seen#{loops => Loops#{step(Dir, At) => true}};

                        _ ->
                            Seen
                    end;

                not_found ->
                    Seen
            end
    end.


find_first_path(_Obstacles, _Seen, {W, H}, _Dir, {R, C}, _Origin, _Visited) when R < 1; C < 1; R > H; C > W ->
    not_found;
find_first_path(Obstacles, Seen, Size, Dir, At, Prev, Visited) ->
    SeenAt = maps:get(At, Seen, not_found),
    ObstacleAt = maps:get(At, Obstacles, not_found),
    case {ObstacleAt, SeenAt, is_map_key(At, Visited)} of
        {not_found, #{Dir := true}, _} ->
            {ok, At};

        {_, _, true} ->
            {ok, At};

        % Turn at an obstacle (could not have visited an obstacle).
        {true, not_found, false} ->
            NewDir = turn(Dir),
            Reflect = turn(NewDir),
            case step(Reflect, At) of
                % Can’t reflect back at self.
                Prev ->
                    not_found;

                Before ->
                    find_first_path(Obstacles, Seen, Size, NewDir, step(NewDir, Before), Before, Visited#{At => true})
            end;

        {not_found, _, _} ->
            find_first_path(Obstacles, Seen, Size, Dir, step(Dir, At), At, Visited#{At => true})
    end.

delta(From, From) -> 0;
delta(From, To) when From < To -> 1;
delta(From, To) when From > To -> -1.

step(up, {R, C}) -> {R - 1, C};
step(down, {R, C}) -> {R + 1, C};
step(left, {R, C}) -> {R, C - 1};
step(right, {R, C}) -> {R, C + 1};

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
    maps:size(Blockers).


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(41, aoc:test(day6, p1, "day6_a")).

p2_test_() -> [
    {"Example", ?_assertEqual(6, aoc:test(day6, p2, "day6_a"))},
    {
        """
        Blockers not only appear when crossing a path, but also when
        passing a point that if turned, would re-join a prior path.
        """,
        ?_assertNotEqual(756, aoc:test(day6, p2, "day6"))
    },
    {
        """
        Can’t place blockers on a spot already traversed, else the
        guard wouldn’t be able to arrive at the current spot.
        """,
        ?_assertNotEqual(714, aoc:test(day6, p2, "day6"))
    },
    {
        """
        If finding a path and an obstacle is in the way, one needs
        to turn before the obstacle and recurse, continue looking
        for a path that would connect to a loop.
        """,
        ?_assertNotEqual(428, aoc:test(day6, p2, "day6"))
    },
    {
        """
        Not sure why this is wrong.
        """,
        ?_assertNotEqual(1338, aoc:test(day6, p2, "day6"))
    },
    {
        """
        Will this be the one?
        """,
        ?_assertNotEqual(1500, aoc:test(day6, p2, "day6"))
    },
    {
        """
        Didn’t get the visitation check right, because I was
        checking if the loop-finder had already visited obstacles,
        which isn’t possible; it should have been checking that
        it wasn’t re-visiting locations that didn’t also have
        a previously-seen traversal.

        But this is also wrong :(
        """,
        ?_assertNotEqual(1339, aoc:test(day6, p2, "day6"))
    }
].

-endif.
