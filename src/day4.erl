-module(day4).
-moduledoc """
Started at 10:00pm
Part 1 solved at 10:37pm
Part 2 solved at 10:52pm
Solved in 52 minutes.

@author Dennis Snell <dmsnell@xkq.io>
@copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
Created : 03. Dec 2024 9:35 PM
""".
-author("Dennis Snell <dmsnell@xkq.io>").
-behavior(aoc).

%% API
-export([config/0]).


config() -> #{
    p1 => {fun p1_submitted/1, lines},
    p2 => {fun p2_submitted/1, lines}
}.


p1_submitted(Lines) ->
    p1(Lines, 0).

p1([], Count) ->
    Count;

p1([L1, L2, L3, L4 | Lines], C) ->
    p1([L2, L3, L4 | Lines], C + matches(0, {L1, L2, L3, L4}));

p1([L1 | Lines], C) ->
    p1(Lines, C + matches(0, {L1})).


matches(Count, {<<>>, <<>>, <<>>, <<>>}) ->
    Count;

matches(Count, {
    <<L1a, L1b, L1c, L1d, _/binary>> = L1,
    <<L2a, L2b, L2c,   _, _/binary>> = L2,
    <<L3a, L3b, L3c,   _, _/binary>> = L3,
    <<L4a,   _,   _, L4d, _/binary>> = L4
}) ->
    WE = <<L1a, L1b, L1c, L1d>>,
    NS = <<L1a, L2a, L3a, L4a>>,
    SE = <<L1a, L2b, L3c, L4d>>,
    NE = <<L4a, L3b, L2c, L1d>>,
    Matches = [1 || S <- [WE, NS, SE, NE], S == <<"XMAS">> orelse S == <<"SAMX">>],
    matches(Count + lists:sum(Matches), {tail(L1), tail(L2), tail(L3), tail(L4)});

matches(Count, {
    <<L1a, L1/binary>>,
    <<L2a, L2/binary>>,
    <<L3a, L3/binary>>,
    <<L4a, L4/binary>>
}) ->
    NS = <<L1a, L2a, L3a, L4a>>,
    Matches = [1 || S <- [NS], S == <<"XMAS">> orelse S == <<"SAMX">>],
    matches(Count + lists:sum(Matches), {L1, L2, L3, L4});

matches(Count, {<<Prefix:4/bytes, _/binary>> = Line}) ->
    Matches = [1 || S <- [Prefix], S == <<"XMAS">> orelse S == <<"SAMX">>],
    matches(Count + lists:sum(Matches), {tail(Line)});

matches(Count, {L}) when is_binary(L), byte_size(L) < 4 ->
    Count.


tail(<<_, Buffer/binary>>) -> Buffer.


p2_submitted(Lines) ->
    p2(Lines, 0).

p2([L1, L2, L3 | Lines], C) ->
    p2([L2, L3 | Lines], C + x_matches(0, {L1, L2, L3}));

p2(_Lines, C) ->
    C.


x_matches(Count, {
    <<L1a,   _, L1c, _/binary>> = L1,
    <<  _, L2b,   _, _/binary>> = L2,
    <<L3a,   _, L3c, _/binary>> = L3
}) ->
    Matches = case <<L1a, L3c, L2b, L3a, L1c>> of
        <<"MSAMS">> -> 1;
        <<"MSASM">> -> 1;
        <<"SMAMS">> -> 1;
        <<"SMASM">> -> 1;
        _ -> 0
    end,
    x_matches(Count + Matches, {tail(L1), tail(L2), tail(L3)});

x_matches(Count, _) ->
    Count.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(18, aoc:test(day4, p1, "day4_a")).

p2_test() ->
    ?assertEqual(9, aoc:test(day4, p2, "day4_a")).

-endif.
