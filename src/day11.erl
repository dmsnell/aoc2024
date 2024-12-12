-module(day11).
-moduledoc """
Started at 10:00pm
Part 1 solved at 10:26pm
Paused at 12:27am, resumed at 8:45am

@author Dennis Snell <dmsnell@xkq.io>
@copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
Created : 10. Dec 2024 9:33 PM
""".
-author("Dennis Snell <dmsnell@xkq.io>").

%% API
-export([config/0]).

config() -> #{
    p1 => {fun p1_submitted/1, raw},
    p2 => {fun p2_submitted/1, raw}
}.

p1_submitted(Data) ->
    Stones = load(Data),
    FinalStones = lists:foldl(
        fun (I, PrevStones) ->
            io:format("\e[90mFolding (\e[33m~p\e[90m): \e[34m~40.ts\e[m\n", [76 - I, lists:join(<<" ">>, PrevStones)]),
            step(PrevStones)
        end,
        Stones,
        lists:seq(1,75)
    ),
    length(FinalStones).


load(Data) ->
    load(Data, []).

load(<<>>, Stones) ->
    lists:reverse(Stones);

load(<<"\n">>, Stones) ->
    load(<<>>, Stones);

load(Buffer, Stones) ->
    case string:take(Buffer, lists:seq($0, $9)) of
        {<<>>, <<" ", Rest/binary>>} ->
            load(Rest, Stones);

        {Stone, Rest} when byte_size(Stone) > 0 ->
            load(Rest, [Stone | Stones])
    end.


step(Stones) ->
    lists:flatten([
        rule(Stone)
        ||
        Stone <- Stones
    ]).

rule(<<"0">>) -> <<"1">>;
rule(Digits) when byte_size(Digits) rem 2 == 0 ->
    Half = byte_size(Digits) div 2,
    Left = binary:part(Digits, 0, Half),
    RightLeading = binary:part(Digits, Half, byte_size(Digits) - Half),
    Right = case string:take(RightLeading, "0") of
        {_Zeros, <<>>} -> <<"0">>;
        {_Zeros, NonZero} -> NonZero
    end,
    [Left, Right];
rule(Digits) ->
    {{N, _L}, <<>>} = day1:int(Digits, 0, 0),
    integer_to_binary(N * 2024).


-doc """
Try: Store a dictionary-like lookup using LZW-like compression.
     For each stone, if it’s not in the dictionary, add it.
     If it is, check if the sequence of it and the next are also there.
     Keep doing this until an arbitrary dictionary size.
""".
p2_submitted(RawData) ->
    % End every number in a space to eliminate the special-case of the last one.
    Data = <<(string:trim(RawData))/binary, " ">>,
    p2_fold(Data, #{}, 75).

p2_fold(Data, _Cache, 0) ->
%%    io:format("\e[90mCache is\n\e[35m~p\e[m\n", [_Cache]),
    count(Data);

p2_fold(Data, Cache, N) ->
    io:format("\e[90mFolding (\e[33m~p\e[90m): \e[2;33m~ts\e[m\n", [N, binary:part(Data, 0, min(40, byte_size(Data)))]),
    {NextData, NextCache} = step(first, Data, Cache, <<>>),
    p2_fold(NextData, NextCache, N - 1).


count(Data) ->
    count(Data, 0).

count(<<>>, Count) ->
    Count;
count(<<" ", Data/binary>>, Count) ->
    count(Data, Count + 1);
count(<<_, Data/binary>>, Count) ->
    count(Data, Count).

step(first, <<>>, Cache, NextBuffer) ->
    {NextBuffer, Cache};

step(first, Buffer, Cache, NextBuffer) ->
    case get_longest_cache(Buffer, Cache) of
        {ok, _Key, Stepped, <<>>} ->
            {<<NextBuffer/binary, Stepped/binary>>, Cache};

        {ok, Key, Stepped, Rest} ->
%%            io:format("\e[32mCached\e[90m: \e[33m~ts\e[90m = \e[35m~ts\e[m\n", [Key, Stepped]),
            step({second, Key, Stepped}, Rest, Cache, NextBuffer);

        not_found ->
            {ok, _Stone, Stepped, Rest, NextCache} = step(no_cache, Buffer, Cache, NextBuffer),
            step(first, Rest, NextCache, <<NextBuffer/binary, Stepped/binary>>)
    end;

step({second, _FirstKey, FirstStepped}, <<>>, Cache, NextBuffer) ->
    {<<NextBuffer/binary, FirstStepped/binary>>, Cache};

step({second, FirstKey, FirstStepped}, Buffer, Cache, NextBuffer) ->
%%    io:format("\e[90m  trying to find next cachable at \e[3;35m~ts\e[m\n", [Buffer]),
    {ok, Stone, Stepped, Rest, NextCache} = step(no_cache, Buffer, Cache, NextBuffer),
    ExtendedKey = <<FirstKey/binary, Stone/binary>>,
    ExtendedStepped = <<FirstStepped/binary, Stepped/binary>>,
    ExtendedBuffer = <<NextBuffer/binary, ExtendedStepped/binary>>,
    ExtendedCache = case map_size(NextCache) of
        S when S < 8192 ->
            maps:put(ExtendedKey, ExtendedStepped, NextCache);

        _ ->
            NextCache
    end,
%%    io:format("\e[3;31mCaching\e[0;90m: \e[33m~ts\e[90m = \e[34m~ts\e[m\n", [ExtendedKey, ExtendedBuffer]),
    step(first, Rest, ExtendedCache, ExtendedBuffer);

step(no_cache, Buffer, Cache, _NextBuffer) ->
    case next_stone(Buffer) of
        not_found ->
            not_found;

        {ok, Stone, Rest} when map_size(Cache) < 8192 ->
            Stepped   = rule2(Stone),
            NextCache = maps:put(Stone, Stepped, Cache),
            {ok, Stone, Stepped, Rest, NextCache};

        {ok, Stone, Rest} ->
            Stepped   = rule2(Stone),
            {ok, Stone, Stepped, Rest, Cache}
    end.

get_longest_cache(Buffer, Cache) ->
    get_longest_cache(Buffer, Cache, <<>>).

get_longest_cache(Buffer, Cache, PrevKey) ->
    case next_stone(Buffer) of
        not_found ->
            case PrevKey of
                <<>> ->
                    not_found;

                _ ->
                    case PrevKey of
                        <<>> ->
                            not_found;

                        _ ->
                            {ok, PrevKey, maps:get(PrevKey, Cache), Buffer}
                    end
            end;

        {ok, Stone, Rest} ->
            NextKey = <<PrevKey/binary, Stone/binary>>,
            case is_map_key(NextKey, Cache) of
                true  -> get_longest_cache(Rest, Cache, NextKey);
                false ->
                    case PrevKey of
                        <<>> ->
                            not_found;

                        _ ->
                            {ok, PrevKey, maps:get(PrevKey, Cache), Buffer}
                    end
            end
    end.

-spec next_stone(Buffer :: binary()) -> not_found | {ok, Digits :: binary(), Rest :: binary()}.
next_stone(Buffer) ->
    case digits(Buffer) of
        {<<>>, Buffer} ->
            not_found;
        {Digits, <<" ", Rest/binary>>} ->
            {ok, <<Digits/binary, " ">>, Rest}
    end.

digits(Buffer) ->
    case digits(Buffer, 0) of
        0 -> {<<>>, Buffer};
        L -> split_binary(Buffer, L)
    end.

digits(<<D, Buffer/binary>>, Length) when D >= $0, D =< $9 ->
    digits(Buffer, Length + 1);

digits(_Buffer, Length) ->
    Length.

rule2(<<"0 ">>) -> <<"1 ">>;
rule2(Digits) when byte_size(Digits) rem 2 == 1 ->
    Half = byte_size(Digits) div 2,
    Left = binary:part(Digits, 0, Half),
    RightLeading = binary:part(Digits, Half, Half),
    Right = case string:take(RightLeading, "0") of
                {_Zeros, <<>>} -> <<"0">>;
                {_Zeros, NonZero} -> NonZero
            end,
    <<Left/binary, " ", Right/binary, " ">>;
rule2(Digits) ->
    {{N, _L}, <<>>} = day1:int(Digits, 0, 0),
    <<(integer_to_binary(N * 2024))/binary, " ">>.

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(55312, aoc:test(day11, p1, "day11_a")).

p2_test() ->
    ?assertEqual(55312, aoc:test(day11, p2, "day11_a")).

-endif.
