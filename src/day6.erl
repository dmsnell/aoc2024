-module(day6).
-moduledoc """
Started at 10:00pm
Part 1 solved at 11:00pm
    - first attempt, 313, was not successful (min/max copy mistake).
Part 2 solved at 2:00am
    - first attempt, 756, was not successful (pretty sure it would be wrong)
    - second, 714, was wrong (thought it was because I had previously added
      blockers where the path had already traversed, but this wasn’t the fix).
Solved in 4 hours.

@author Dennis Snell <dmsnell@xkq.io>
@copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
Created : 05. Dec 2024 8:21 PM
""".
-author("Dennis Snell <dmsnell@xkq.io>").

%% API
-export([config/0]).

-export([step/2]).

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


navigate(Size, Obstacles, Guard) ->
    navigate(Size, Obstacles, Guard, no_probe).

navigate({_H, _W} = Size, Obstacles, {Row, Col, _Dir} = Guard, Probe) ->
    navigate(Size, Obstacles, Guard, log_new(Obstacles, Size, {Row, Col}, up), Probe).

navigate({H, W} = Size, Obstacles, {Row, Col, Dir} = _Guard, Traversed, Probe) ->
    From = {Row, Col},
    case Traversed of
        #{{Row, Col} := #{Dir := true}} ->
            loop;

        _ ->
            case Dir of
                up ->
                    case [R || {R, C} := _ <- Obstacles, C == Col, R < Row] of
                        [] ->
                            {{1, Col}, traverse(Traversed, Dir, From, {1, Col}, Probe)};
                        InWay ->
                            R = lists:max(InWay),
                            navigate(
                                Size,
                                Obstacles,
                                {R + 1, Col, turn(Dir)},
                                traverse(Traversed, Dir, From, {R + 1, Col}, Probe),
                                Probe
                            )
                    end;

                down ->
                    case [R || {R, C} := _ <- Obstacles, C == Col, R > Row] of
                        [] ->
                            {{H, Col}, traverse(Traversed, Dir, From, {H, Col}, Probe)};
                        InWay ->
                            R = lists:min(InWay),
                            navigate(
                                Size,
                                Obstacles,
                                {R - 1, Col, turn(Dir)},
                                traverse(Traversed, Dir, From, {R - 1, Col}, Probe),
                                Probe
                            )
                    end;

                left ->
                    case [C || {R, C} := _ <- Obstacles, R == Row, C < Col] of
                        [] ->
                            {{Row, 1}, traverse(Traversed, Dir, From, {Row, 1}, Probe)};
                        InWay ->
                            C = lists:max(InWay),
                            navigate(
                                Size,
                                Obstacles,
                                {Row, C + 1, turn(Dir)},
                                traverse(Traversed, Dir, From, {Row, C + 1}, Probe),
                                Probe
                            )
                    end;

                right ->
                    case [C || {R, C} := _ <- Obstacles, R == Row, C > Col] of
                        [] ->
                            {{Row, W}, traverse(Traversed, Dir, From, {Row, W}, Probe)};
                        InWay ->
                            C = lists:min(InWay),
                            navigate(
                                Size,
                                Obstacles,
                                {Row, C - 1, turn(Dir)},
                                traverse(Traversed, Dir, From, {Row, C - 1}, Probe),
                                Probe
                            )
                    end
            end
    end.


traverse(Seen, Dir, {FR, FC} = From, {TR, TC} = To, Probe) ->
    Delta = {delta(FR, TR), delta(FC, TC)},
    traverse(
        log(Seen, From, Dir, Probe),
        Dir,
        Delta,
        step(Delta, From),
        To,
        Probe
    ).

traverse(Seen, Dir, _Delta, To, To, Probe) ->
    log(Seen, To, Dir, Probe);
traverse(Seen, Dir, Delta, From, To, Probe) ->
    traverse(log(Seen, From, Dir, Probe), Dir, Delta, step(Delta, From), To, Probe).

log_new(Obstacles, Size, {_R, _C} = _At, _Dir) ->
    #{
%%        {R, C} => #{Dir => true},
        size => Size,
        loops => #{},
        obstacles => Obstacles
    }.

log(Seen, At, Dir, Probe) ->
    maps:update_with(
        At,
        fun (Dirs) -> Dirs#{Dir => true} end,
        #{Dir => true},
        case Probe of
            probing ->
                Seen;

            no_probe ->
                find_blocker(Seen, At, Dir)
        end
    ).

find_blocker(#{loops := Loops, size := Size, obstacles := Obstacles} = Seen, {Row, Col} = At, Dir) ->
    BlockerAt = step(Dir, At),
    case is_map_key(BlockerAt, Seen) of
        true ->
            Seen;

        false ->
            case navigate(Size, Obstacles#{BlockerAt => true}, {Row, Col, Dir}, probing) of
                loop ->
                    Seen#{loops => Loops#{BlockerAt => true}};

                _ ->
                    Seen
            end
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
    {"Example", ?_assertEqual(6, aoc:test(day6, p2, "day6_a"))}
%%    Commented out because these are slow.
%%    {
%%        """
%%        Blockers not only appear when crossing a path, but also when
%%        passing a point that if turned, would re-join a prior path.
%%        """,
%%        ?_assertNotEqual(756, aoc:test(day6, p2, "day6"))
%%    },
%%    {
%%        """
%%        Can’t place blockers on a spot already traversed, else the
%%        guard wouldn’t be able to arrive at the current spot.
%%        """,
%%        ?_assertNotEqual(714, aoc:test(day6, p2, "day6"))
%%    },
%%    {
%%        """
%%        If finding a path and an obstacle is in the way, one needs
%%        to turn before the obstacle and recurse, continue looking
%%        for a path that would connect to a loop.
%%        """,
%%        ?_assertNotEqual(428, aoc:test(day6, p2, "day6"))
%%    },
%%    {
%%        """
%%        Not sure why this is wrong.
%%        """,
%%        ?_assertNotEqual(1338, aoc:test(day6, p2, "day6"))
%%    },
%%    {
%%        """
%%        Will this be the one?
%%        """,
%%        ?_assertNotEqual(1500, aoc:test(day6, p2, "day6"))
%%    },
%%    {
%%        """
%%        Didn’t get the visitation check right, because I was
%%        checking if the loop-finder had already visited obstacles,
%%        which isn’t possible; it should have been checking that
%%        it wasn’t re-visiting locations that didn’t also have
%%        a previously-seen traversal.
%%
%%        But this is also wrong :(
%%        """,
%%        ?_assertNotEqual(1339, aoc:test(day6, p2, "day6"))
%%    },
%%    {
%%        """
%%        Tried to reuse navigate(), which took a long time,
%%        but still failed.
%%
%%        Probably at this point I need to create a state variable
%%        and rethink the problem. Something is _right_ though about
%%        checking Seen, Visited, and Obstacles. I just can’t think
%%        it through properly at 1:40am.
%%        """,
%%        ?_assertNotEqual(1942, aoc:test(day6, p2, "day6"))
%%    }
].

-endif.
