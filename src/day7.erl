-module(day7).
-moduledoc """
Started at 10:00pm
Part 1 solved at 10:12pm
Part 2 solved at 10:18pm
Solved in 18 minutes.

@author Dennis Snell <dmsnell@xkq.io>
@copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
Created : 06. Dec 2024 9:58 PM
""".
-author("Dennis Snell <dmsnell@xkq.io>").

%% API
-export([config/0]).

config() -> #{
    p1 => {fun p1_submitted/1, lines},
    p2 => {fun p2_submitted/1, lines}
}.

p1_submitted(Lines) ->
    lists:foldl(fun (Line, Sum) -> Sum + line(Line) end, 0, Lines).


line(Line) ->
    {{TestValue, _}, <<":", BeforeFirst/binary>>} = day1:int(Line, 0, 0),
    {{FirstNum, _}, Rest} = day1:int(day1:trim(BeforeFirst), 0, 0),
    test(TestValue, FirstNum, Rest).

test(TestValue, Total, <<>>) ->
    case Total of
        TestValue -> TestValue;
        _         -> 0
    end;

test(TestValue, Total, _Buffer) when Total > TestValue ->
    0;

test(TestValue, Total, Buffer) ->
    {{Next, _}, Rest} = day1:int(Buffer, 0, 0),
    case test(TestValue, Total + Next, Rest) of
        0 ->
            test(TestValue, Total * Next, Rest);

        N ->
            N
    end.


p2_submitted(Lines) ->
    lists:foldl(fun (Line, Sum) -> Sum + line2(Line) end, 0, Lines).

line2(Line) ->
    {{TestValue, _}, <<":", BeforeFirst/binary>>} = day1:int(Line, 0, 0),
    {{FirstNum, _}, Rest} = day1:int(day1:trim(BeforeFirst), 0, 0),
    test2(TestValue, FirstNum, Rest).

test2(TestValue, Total, <<>>) ->
    case Total of
        TestValue -> TestValue;
        _         -> 0
    end;

test2(TestValue, Total, _Buffer) when Total > TestValue ->
    0;

test2(TestValue, Total, Buffer) ->
    {{Next, _}, Rest} = day1:int(Buffer, 0, 0),
    case test2(TestValue, Total + Next, Rest) of
        0 ->
            case test2(TestValue, Total * Next, Rest) of
                0 ->
                    % These are all ASCII, so unicode:characters_to_binary isn’t necessary.
                    Joined = list_to_binary(io_lib:format("~p~p", [Total, Next])),
                    test2(TestValue, binary_to_integer(Joined), Rest);

                N ->
                    N
            end;

        N ->
            N
    end.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(3749, aoc:test(day7, p1, "day7_a")).

p2_test() ->
    ?assertEqual(11387, aoc:test(day7, p2, "day7_a")).

-endif.
