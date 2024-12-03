-module(day3).
-moduledoc """
Started at 10:02pm
Part 1 solved in 9 minutes at 10:10pm.
Part 2 solved in 11 minutes at 10:22pm.
Solved in twenty minutes.

@author Dennis Snell <dmsnell@xkq.io>
@copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>

Created : 02. Dec 2024 9:19 PM
""".
-author("Dennis Snell <dmsnell@xkq.io>").

%% API
-export([config/0]).

config() -> #{
    p1 => {fun p1_submitted/1, raw},
    p2 => {fun p2_submitted/1, raw}
}.


-doc """
Submitted solution.

#### Benchmark

```
    #{total => {32138.917,ms},
      answer => 163931492,
      measured => {avg,{0.287,ms},min,{0.227,ms}},
      total_per => {0.321,ms}}
```
""".
p1_submitted(Data) ->
    p1_parse(instruction, Data, 0).

p1_parse(_, <<>>, Sum) ->
    Sum;

p1_parse(instruction, Data, Sum) ->
    case binary:split(Data, <<"mul(">>) of
        [_Rest] -> Sum;
        [_Prefix, Rest] -> p1_parse(int1, Rest, Sum)
    end;

p1_parse(int1, Data, Sum) ->
    case day1:int(Data, 0, 0) of
        {{Int1, Length}, <<",", Rest/binary>>} when Length > 0 ->
            p1_parse({int2, Int1}, Rest, Sum);

        _ ->
            p1_parse(instruction, Data, Sum)
    end;

p1_parse({int2, Int1}, Data, Sum) ->
    case day1:int(Data, 0, 0) of
        {{Int2, Length}, <<")", Rest/binary>>} when Length > 0 ->
            p1_parse(instruction, Rest, Sum + Int1 * Int2);

        _ ->
            p1_parse(instruction, Data, Sum)
    end.


-doc """
Submitted solution

#### Benchmark

```
    #{total => {18415.56,ms},
      answer => 76911921,
      measured => {avg,{0.155,ms},min,{0.101,ms}},
      total_per => {0.184,ms}}
```

I modified this to skip ahead to the nearest `d` or `m` using
`string:take(Data, "dm", true)` and then matching on the suffix
of that split, but it took the runtime from 100 µs to about 1 ms.
""".
p2_submitted(Data) ->
    p2_parse(instruction, Data, {on, 0}).

p2_parse(_, <<>>, {_OnOff, Sum}) ->
    Sum;


p2_parse(instruction, Data, {off, Sum}) ->
    case binary:split(Data, <<"do()">>) of
        [_Rest] ->
            Sum;

        [_Prefix, Rest] ->
            p2_parse(instruction, Rest, {on, Sum})
    end;

p2_parse(instruction, Data, {on, Sum}) ->
    case Data of
        <<"don't()", Rest/binary>> ->
            p2_parse(instruction, Rest, {off, Sum});

        <<"mul(", Rest/binary>> ->
            p2_parse(int1, Rest, {on, Sum});

        <<_, Rest/binary>> ->
            p2_parse(instruction, Rest, {on, Sum})
    end;

p2_parse(int1, Data, {on, Sum}) ->
    case day1:int(Data, 0, 0) of
        {{Int1, Length}, <<",", Rest/binary>>} when Length > 0 ->
            p2_parse({int2, Int1}, Rest, {on, Sum});

        _ ->
            p2_parse(instruction, Data, {on, Sum})
    end;

p2_parse({int2, Int1}, Data, {on, Sum}) ->
    case day1:int(Data, 0, 0) of
        {{Int2, Length}, <<")", Rest/binary>>} when Length > 0 ->
            p2_parse(instruction, Rest, {on, Sum + Int1 * Int2});

        _ ->
            p2_parse(instruction, Data, {on, Sum})
    end.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(161, aoc:test(day3, p1, "day3_a")).

p2_test() ->
    ?assertEqual(48, aoc:test(day3, p2, "day3_b")).

-endif.
