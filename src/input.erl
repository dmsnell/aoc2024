%%%-------------------------------------------------------------------
%%% @author Dennis Snell <dmsnell@xkq.io>
%%% @copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
%%% @doc
%%%
%%% @end
%%% Created : 21. Nov 2024 3:36 PM
%%%-------------------------------------------------------------------
-module(input).
-author("dmsnell").

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    groups_and_lines/1,
    io_device/1,
    lines/1,
    number_list/1,
    numbers/1,
    raw/1
]).

-spec groups_and_lines(Name :: string()) -> list(list(binary())).

groups_and_lines(Name) ->
    Groups = binary:split(raw(Name), [<<"\n\n">>], [global]),
    [[binary_to_list(Form) || Form <- binary:split(Group, [<<"\n">>], [global])] || Group <- Groups].

-spec raw(Name :: string()) -> Data :: binary().

raw(Name) ->
    Path = filename:join([code:priv_dir(aoc2024), Name ++ ".txt"]),
    case file:read_file(Path) of
        {ok, Data} ->
            Data;
        {error, enoent} ->
            {ok, Data} = fetch_day_input(Name),
            file:write_file(Path, Data),
            Data
    end.

-spec io_device(Name :: unicode:chardata()) -> {io_device, file:io_device()}.

io_device(Name) ->
    Path = filename:join([code:priv_dir(aoc2024), unicode:characters_to_list([Name, ".txt"])]),
    {ok, IODevice} = file:open(Path, [binary, raw, read, read_ahead]),
    {io_device, IODevice}.

-spec lines(Name :: string()) -> Lines :: list(binary()).

lines(Name) ->
    binary:split(raw(Name), [<<"\r\n">>, <<"\n">>], [global, trim]).

-spec number_list(Name :: string()) -> Numbers :: list(integer()).

number_list(Name) ->
    lists:map(fun binary_to_integer/1, lines(Name)).

numbers(Name) ->
    lists:map(fun binary_to_integer/1, binary:split(raw(Name), <<",">>, [global])).


fetch_day_input(Day) ->
    "day" ++ N = Day,
    Path = ["https://adventofcode.com/2024/day/", N, "/input"],
    Response = httpc:request(
        get,
        {
            Path,
            [{"Cookie", session_cookie()}]
        },
        [{ssl, ssl_options()}],
        [{body_format, binary}]
    ),
    case Response of
        {ok, {{HttpType, StatusCode, Status}, Headers, Body}} ->
            LongestHeader = lists:max([string:length(Name) || {Name, _Value} <- Headers]),
            PrintHeaders  = [io_lib:format("    \e[3;34m~*.ts\e[0;90m: \e[35m~ts\e[m~n", [LongestHeader, Name, Value]) || {Name, Value} <- Headers],
            BodyPreview   = format_preview(Body, 80, ~"    "),
            ?LOG_DEBUG(
                ~b"""
                    \e[90mHTTP Call: \e[33m~ts\e[m
                    \e[35m~p ~ts\e[34m ~ts\e[m

                    ~ts\e[m
                    \e[3;36m~ts\e[m
                    """,
                [Path, StatusCode, Status, HttpType, PrintHeaders, BodyPreview]
            )
    end,
    case Response of
        {ok, {{_, 200, _}, _Headers, Data}} ->
            {ok, Data}
    end.


format_preview(Body, GraphemesPerLine, LinePrefix) ->
    format_preview([], Body, string:slice(Body, 0, GraphemesPerLine), GraphemesPerLine, LinePrefix).

format_preview(Output, _, <<>>, _GraphemesPerLine, _LinePrefix) ->
    lists:reverse(Output);
format_preview(Output, BinaryBody, Slice, GraphemesPerLine, LinePrefix) ->
    {_Prefix, NextChunk} = split_binary(BinaryBody, byte_size(Slice)),
    format_preview([["\n", LinePrefix, Slice] | Output], NextChunk, string:slice(NextChunk, 0, GraphemesPerLine), GraphemesPerLine, LinePrefix).


session_cookie() ->
    {ok, Cookie} = file:read_file(filename:join(code:priv_dir(aoc2024), ".aoc-session.cookie")),
    Cookie.


ssl_options() ->
    [
        {verify, verify_peer},
        {cacerts, public_key:cacerts_get()},
        {customize_hostname_check, [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}
    ].
