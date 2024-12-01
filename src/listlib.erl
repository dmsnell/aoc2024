%%%-------------------------------------------------------------------
%%% @author Dennis Snell <dmsnell@xkq.io>
%%% @copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
%%% @doc
%%%
%%% @end
%%% Created : 21. Nov 2024 3:35 PM
%%%-------------------------------------------------------------------
-module(listlib).
-author("dmsnell").

%% API
-export([
    chunks/2
]).


chunks(N, List) when is_list(List) ->
    chunks(N, List, []).

chunks(N, [<<>>], Chunks) ->
    chunks(N, [], Chunks);
chunks(_N, [], Chunks) ->
    lists:reverse(Chunks);

chunks(N, List, Chunks) ->
    {Head, Tail} = lists:split(N, List),
    chunks(N, Tail, [Head | Chunks]).
