%%%-------------------------------------------------------------------
%%% @author Dennis Snell <dmsnell@xkq.io>
%%% @copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
%%% @doc
%%%
%%% @end
%%% Created : 21. Nov 2024 4:45 PM
%%%-------------------------------------------------------------------
-module(day1).
-author("dmsnell").
-behavior(aoc).

-export([config/0]).
-export([int/3, trim/1]).

config() -> #{
    p1 => {fun p1/1, lines},
    p2 => {fun p2_draft4/1, lines},

    p1_submitted => {fun p1_submitted/1, lines},
    p2_submitted => {fun p2_submitted/1, lines},

    p2_draft2 => {fun p2_draft2/1, lines},
    p2_draft3 => {fun p2_draft3/1, lines},
    p2_draft4 => {fun p2_draft4/1, lines},
    p2_draft5 => {fun p2_draft5/1, io_device}
}.

-doc """
Submitted solution.

#### Benchmark

```
    #{total => {78377.212,ms},
      measured => {avg,{0.707,ms},min,{0.544,ms}},
      total_per => {0.784,ms}}
```
""".
p1_submitted(Lines) ->
    {Left, Right} = lists:unzip([parse_line(Line) || Line <- Lines, byte_size(Line) > 0]),
    SortedLeft = lists:sort(Left),
    SortedRight = lists:sort(Right),
    Rezipped = lists:zip(SortedLeft, SortedRight),
    Diffs = [abs(L - R) || {L, R} <- Rezipped],
    lists:sum(Diffs).

parse_line(Line) when is_binary(Line) ->
    {Left, Rest} = string:to_integer(Line),
    {_WS, RightString} = string:take(Rest, [$ ]),
    {Right, <<>>} = string:to_integer(RightString),
    {Left, Right}.


-doc """
Optimized by inlining the calculation of diffs.

#### Benchmark

As expected, the increase in speed was marginal, though measureable.

```
    #{total => {74973.51,ms},
      measured => {avg,{0.685,ms},min,{0.531,ms}},
      total_per => {0.75,ms}}
```
""".
p1(Lines) ->
    {Left, Right} = lists:unzip([parse_line(Line) || Line <- Lines, byte_size(Line) > 0]),
    SortedLeft = lists:sort(Left),
    SortedRight = lists:sort(Right),
    p1_sum(0, SortedLeft, SortedRight).

p1_sum(Sum, [], []) ->
    Sum;

p1_sum(Sum, [Left | Lefts], [Right | Rights]) ->
    p1_sum(Sum + abs(Left - Right), Lefts, Rights).

-doc """
Submitted solution.

#### Benchmark

Runs in around 1.0ms

```
    #{total => {8668.28,ms},
      measured => {avg,{0.801,ms},min,{0.658,ms}},
      total_per => {0.867,ms}}
```
""".
p2_submitted(Lines) ->
    {Left, Right} = lists:unzip([parse_line(Line) || Line <- Lines, byte_size(Line) > 0]),
    SortedRight = lists:sort(Right),
    Counts = lists:foldl(
        fun (V, C) -> maps:update_with(V, fun (A) -> A + 1 end, 1, C) end,
        #{},
        SortedRight
    ),
    lists:sum([K * maps:get(K, Counts, 0) || K <- Left]).

-doc """
Optimized solution.

This one performs the counting as it parses the list rather than
reading in two full lists, then counting the right, then iterating.
It also stores the counts of the numbers on the left so that the
final iteration to sum up the values occurs faster, requiring only
a single iteration for each number appearing in the left.

#### Updates

  - The first optimization was slower than the submitted solution,
    where I took the counts of the left side also in each step.

  - For the second optimization I reverted the left numbers back
    into a list and iterated at the end. This was faster than both
    the submitted and initial optimization. I guess that it’s
    saving work by multiplying and adding duplicates at the end
    vs. computing the hashes for the map. Is it possible to make
    the right half with its counts go faster?

#### Benchmark

This version is not faster than the submitted version.
In fact, it’s slower.

With the left counts as a list instead of a map of counts.

```
    #{total => {34299.489,ms},
      measured => {avg,{0.277,ms},min,{0.196,ms}},
      total_per => {0.343,ms}}
```

""".
p2_draft2(Lines) ->
    {Lefts, Rights} = lists:foldl(
        fun (Line, Counts) -> p2_line(Counts, Line) end,
        {[], #{}},
        [Line || Line <- Lines, byte_size(Line) > 0]
    ),
    lists:sum([L * maps:get(L, Rights, 0) || L <- Lefts]).

inc(A) ->
    A + 1.

p2_line({LeftCounts, RightCounts}, Line) ->
    {Left, WS} = string:to_integer(Line),
    {_, RightHalf} = string:take(WS, " "),
    {Right, <<>>} = string:to_integer(RightHalf),
    {
        [Left | LeftCounts],
        maps:update_with(Right, fun inc/1, 1, RightCounts)
    }.

-doc """
It’s only important to parse the numbers on the left side, not
on the right. The only thing that matters on the right is that
unique sequences are counted, which can include the spacing.

Also skip the list comprehension in case generating the big list
causes unnecessary allocation and time.

#### Benchmark

Skipping the second integer conversion seemed to help a lot. A
further idea could be to skip that map data structure and instead
implement a Trie or other manual tree, but since we don’t have
mutable data structures I think this could be difficult to do
efficiently. Each tier only needs ten slots, and each slot contains
another tree or a leaf count.

```
    #{total => {3596.766,ms},
      measured => {avg,{0.29,ms},min,{0.207,ms}},
      total_per => {0.36,ms}}
```
""".
p2_draft3(Lines) ->
    {Lefts, Rights} = lists:foldl(
        fun (Line, {LeftCounts, RightCounts}) ->
            {{Left, Length}, Rest} = int(Line, 0, 0),
            {[{Left, binary:part(Line, 0, Length)} | LeftCounts], maps:update_with(Rest, fun inc/1, 1, RightCounts)}
        end,
        {[], #{}},
        [Line || Line <- Lines, byte_size(Line) > 0 ]
    ),
    lists:foldl(
        fun ({Id, Key}, Sum) -> Sum + Id * maps:get(Key, Rights, 0) end,
        0,
        Lefts
    ).

-doc """
This is probably faster than string:to_integer() because we know
that we’ll only receive digits 1-9 and that we’ll get at least one.
""".
int(<<C, Rest/binary>>, Accumulator, Length) when C >= $0, C =< $9 ->
    int(Rest, Accumulator * 10 + (C - $0), Length + 1);
int(Buffer, Accumulator, Length) ->
    {{Accumulator, Length}, trim(Buffer)}.

trim(<<C, Rest/binary>>) when C == 16#20 ->
    trim(Rest);
trim(Buffer) ->
    Buffer.

-doc """
No int-parsing, custom tree.

#### Benchmark

```
    #{total => {34678.403,ms},
      measured => {avg,{0.28,ms},min,{0.169,ms}},
      total_per => {0.347,ms}}
```
""".
p2_draft4(Lines) ->
    {Lefts, Rights} = lists:foldl(
        fun (Line, {LeftCounts, RightCounts}) ->
            {{Left, Length}, Rest} = int(Line, 0, 0),
            {[{Left, binary:part(Line, 0, Length)} | LeftCounts], nt_add(RightCounts, Rest)}
        end,
        {[], nt_new()},
        [Line || Line <- Lines, byte_size(Line) > 0 ]
    ),
    lists:foldl(
        fun ({Id, Key}, Sum) -> Sum + Id * nt_get(Rights, Key, 0) end,
        0,
        Lefts
    ).

-doc """
Skip splitting the input into lines and parse directly.

#### Benchmark

Slower than processing lines! At first I used a file:read/2 of 4,096 bytes,
which results in runtimes of around 1.5 ms. Changing to 64 * 4096 cut the
runtime to a third of its value. Increasing to 1024 * 4096 left it pretty
much at the same runtime as 64 * 4096.

```
    #{total => {6240.722,ms},
      measured => {avg,{0.598,ms},min,{0.455,ms}},
      total_per => {0.624,ms}}
```
""".
p2_draft5({io_device, Input}) ->
    p2_step(next, Input, <<>>, {[], nt_new()}).

% Sum up the counts.
p2_step(finish, {Left, Right}) ->
    lists:foldl(
        fun ({L, Key}, Sum) -> Sum + L * nt_get(Right, Key, 0) end,
        0,
        Left
    ).

% Get next chunk of input.
p2_step(next, Input, Buffer, Counts) ->
    p2_step(join, Input, {Buffer, file:read(Input, 64 * 4096)}, Counts);

p2_step(join, Input, {_Buffer, eof}, Counts) ->
    file:close(Input),
    p2_step(finish, Counts);

p2_step(join, Input, {<<>>, {ok, Next}}, Counts) ->
    p2_step(process, Input, Next, Counts);

p2_step(join, Input, {Prev, {ok, Next}}, Counts) ->
    p2_step(process, Input, <<Prev/binary, Next/binary>>, Counts);

% Parse and process the lines.
p2_step(process, Input, Buffer, Counts) ->
    p2_step({left, 0, 0, Buffer}, Input, Buffer, Counts);

p2_step({left, A, Length, Prev}, Input, Buffer, {Left, Right} = Counts) ->
    case Buffer of
        <<C, Rest/binary>> when C >= $0, C =< $9 ->
            p2_step({left, A * 10 + C - $0, Length + 1, Prev}, Input, Rest, Counts);

        <<>> ->
            p2_step(next, Input, Prev, Counts);

        Rest ->
            p2_step({right, Prev}, Input, trim(Rest), {[{A, binary:part(Prev, 0, Length)} | Left], Right})
    end;

p2_step({right, Prev}, Input, Buffer, {Left, Right} = Counts) ->
    case digits(Buffer, 0, Buffer) of
        {A, <<"\n", Rest/binary>>} ->
            p2_step(process, Input, Rest, {Left, nt_add(Right, A)});

        _Rest ->
            p2_step(next, Input, Prev, Counts)
    end.


digits(Buffer, Length, <<C, Rest/binary>>) when C >= $0, C =< $9 ->
    digits(Buffer, Length + 1, Rest);
digits(Buffer, Length, Rest) ->
    {binary:part(Buffer, 0, Length), Rest}.


nt_new() ->
    {nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, 1}.

% Terminal case - update the counter.
nt_add(NT, <<Digit>>) ->
    I = Digit - $/,
    Next = case element(I, NT) of
        nil     -> nt_new();
        Subtree -> setelement(11, Subtree, element(11, Subtree) + 1)
    end,
    setelement(I, NT, Next);

% Non-terminal - descend.
nt_add(NT, <<Digit, Rest/binary>>) ->
    I = Digit - $/,
    Next = case element(I, NT) of
       nil     -> nt_new();
       Subtree -> Subtree
    end,
    setelement(I, NT, nt_add(Next, Rest)).


nt_get(NT, <<>>, _Default) ->
    element(11, NT);
nt_get(NT, <<Digit, Rest/binary>>, Default) ->
    case element(Digit - $/, NT) of
        nil     -> Default;
        Subtree -> nt_get(Subtree, Rest, Default)
    end.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(11, aoc:test(day1, p1, "day1_a")).

p2_test() ->
    ?assertEqual(31, aoc:test(day1, p2, "day1_a")).

-endif.
