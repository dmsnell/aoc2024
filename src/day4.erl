-module(day4).
-moduledoc """
Started at 10:00pm
Part 1 solved at 10:37pm
Part 2 solved at 10:52pm
Solved in 52 minutes.

After submitting, I elided some logical checked to optimize
but didn’t change the overall algorithm. To avoid copying the
entire functions as an optimized version, I made the edits in
place. For the submitted versions check the source history.

@author Dennis Snell <dmsnell@xkq.io>
@copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
Created : 03. Dec 2024 9:35 PM
""".
-author("Dennis Snell <dmsnell@xkq.io>").
-behavior(aoc).

%% API
-export([config/0]).


config() -> #{
    p1 => {fun p1_optimized_logic/1, lines},
    p2 => {fun p2_optimized_logic/1, lines},

    p1_as_binary => {fun p1_as_binary/1, raw}
}.


-doc """
This is a variation of what was submitted with some logical conditions
elided. It’s faster than the submitted version but doesn’t change the
general algorithm or approach.

#### Benchmark

```
#{total => {36346.308,ms},
  answer => 2685,
  measured => {avg,{3.534,ms},min,{2.574,ms}},
  total_per => {3.635,ms}}
```
""".
p1_optimized_logic(Lines) ->
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
    L1A = L1a == $S orelse L1a == $X,
    L1ACount = case L1A of
        false -> 0;
        true  ->
            WE = <<L1a, L1b, L1c, L1d>>,
            NS = <<L1a, L2a, L3a, L4a>>,
            SE = <<L1a, L2b, L3c, L4d>>,
            lists:sum([1 || S <- [WE, NS, SE], S == <<"XMAS">> orelse S == <<"SAMX">>])
    end,
    L4ACount = case <<L4a, L3b, L2c, L1d>> of
        <<"XMAS">> -> 1;
        <<"SAMX">> -> 1;
        _          -> 0
    end,
    matches(Count + L1ACount + L4ACount, {tail(L1), tail(L2), tail(L3), tail(L4)});

matches(Count, {
    <<L1a, L1/binary>>,
    <<L2a, L2/binary>>,
    <<L3a, L3/binary>>,
    <<L4a, L4/binary>>
}) ->
    Matches = case <<L1a, L2a, L3a, L4a>> of
        <<"XMAS">> -> 1;
        <<"SAMX">> -> 1;
        _          -> 0
    end,
    matches(Count + Matches, {L1, L2, L3, L4});

matches(Count, {<<Prefix:4/bytes, _/binary>> = Line}) ->
    Matches = case Prefix of <<"XMAS">> -> 1; <<"SAMX">> -> 1; _ -> 0 end,
    matches(Count + Matches, {tail(Line)});

matches(Count, {L}) when is_binary(L), byte_size(L) < 4 ->
    Count.


tail(<<_, Buffer/binary>>) -> Buffer.


-doc """
Process input as a giant binary with X/Y addressing instead of
splitting into lines of binaries. More imperative approach for
faster runtime.

This turned out to be slower than the optimized logic for the
submitted part. The pattern matching is clearer and faster.

I’m assuming this is because f:rc/4 performs new allocations of
sub-binaries while the pattern-matching version knows it can
reuse the existing one, but this is a guess.

#### Benchmark

```
#{total => {38940.065,ms},
  answer => 2685,
  measured => {avg,{3.845,ms},min,{2.756,ms}},
  total_per => {3.894,ms}}
```
""".
p1_as_binary(Data) ->
    {Size, _} = binary:match(Data, <<"\n">>),
    matches_bin(Data, 0, {full, Size, 0, 0}).


matches_bin(_Data, Count, {full, Size, Size, Size}) ->
    Count;

matches_bin(Data, Count, {full, Size, Row, Size}) ->
    matches_bin(Data, Count, {full, Size, Row + 1, 0});

matches_bin(Data, Count, {full, Size, Row, Col}) when Row + 3 < Size, Col + 3 < Size ->
    <<L1a, L1b, L1c, L1d>> = rc(Data, Size, Row + 0, Col, 4),
    <<L2a, L2b, L2c>>      = rc(Data, Size, Row + 1, Col, 3),
    <<L3a, L3b, L3c>>      = rc(Data, Size, Row + 2, Col, 3),
    <<L4a,   _,   _, L4d>> = rc(Data, Size, Row + 3, Col, 4),
    L1A = L1a == $S orelse L1a == $X,
    L1ACount = case L1A of
                   false -> 0;
                   true  ->
                       WE = <<L1a, L1b, L1c, L1d>>,
                       NS = <<L1a, L2a, L3a, L4a>>,
                       SE = <<L1a, L2b, L3c, L4d>>,
                       lists:sum([1 || S <- [WE, NS, SE], S == <<"XMAS">> orelse S == <<"SAMX">>])
               end,
    L4ACount = case <<L4a, L3b, L2c, L1d>> of
                   <<"XMAS">> -> 1;
                   <<"SAMX">> -> 1;
                   _          -> 0
               end,
    matches_bin(Data, Count + L1ACount + L4ACount, {full, Size, Row, Col + 1});

matches_bin(Data, Count, {full, Size, Row, Col}) when Row + 3 < Size ->
    <<L1a>> = rc(Data, Size, Row + 0, Col, 1),
    <<L2a>> = rc(Data, Size, Row + 1, Col, 1),
    <<L3a>> = rc(Data, Size, Row + 2, Col, 1),
    <<L4a>> = rc(Data, Size, Row + 3, Col, 1),
    Matches = case <<L1a, L2a, L3a, L4a>> of
                  <<"XMAS">> -> 1;
                  <<"SAMX">> -> 1;
                  _          -> 0
              end,
    matches_bin(Data, Count + Matches, {full, Size, Row, Col + 1});

matches_bin(Data, Count, {full, Size, Row, _Col}) ->
    matches_bin(Data, Count, {row, Size, Row, 0});

matches_bin(_Data, Count, {row, Size, Size, _Col}) ->
    Count;

matches_bin(Data, Count, {row, Size, Row, Col}) when Col + 3 >= Size ->
    matches_bin(Data, Count, {row, Size, Row + 1, 0});

matches_bin(Data, Count, {row, Size, Row, Col}) when Row < Size, Col + 3 < Size ->
    Matches = case rc(Data, Size, Row, Col, 4) of <<"XMAS">> -> 1; <<"SAMX">> -> 1; _ -> 0 end,
    matches_bin(Data, Count + Matches, {row, Size, Row, Col + 1}).


rc(Data, Size, Row, Col, Stride) ->
    At = (Size + 1) * Row + Col,
    binary:part(Data, At, Stride).


-doc """
This is a variation of what was submitted with some logical conditions
elided. It’s faster than the submitted version but doesn’t change the
general algorithm or approach.
#### Benchmark

```
#{total => {21257.776,ms},
  answer => 2048,
  measured => {avg,{2.033,ms},min,{1.405,ms}},
  total_per => {2.126,ms}}
```
""".
p2_optimized_logic(Lines) ->
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
    case L2b of
        $A ->
            Matches = case <<L1a, L3c, L2b, L3a, L1c>> of
                <<"MSAMS">> -> 1;
                <<"MSASM">> -> 1;
                <<"SMAMS">> -> 1;
                <<"SMASM">> -> 1;
                _ -> 0
            end,
            x_matches(Count + Matches, {tail(L1), tail(L2), tail(L3)});

        _ ->
            x_matches(Count, {tail(L1), tail(L2), tail(L3)})
    end;

x_matches(Count, _) ->
    Count.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test_() -> [
    {"Optimized logic", ?_assertEqual(18, aoc:test(day4, p1, "day4_a"))},
    {"As binary", ?_assertEqual(18, aoc:test(day4, p1_as_binary, "day4_a"))}
].

p2_test() ->
    ?assertEqual(9, aoc:test(day4, p2, "day4_a")).

-endif.
