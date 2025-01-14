%%%-------------------------------------------------------------------
%%% @author Dennis Snell <dmsnell@xkq.io>
%%% @copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
%%% @doc
%%%
%%% @end
%%% Created : 21. Nov 2024 3:33 PM
%%%-------------------------------------------------------------------
-module(utils).
-author("dmsnell").

%% API
-export([
    count/3,
    isolated/1,
    isolated/2,
    shuffle/1,
    split_by/2
]).

count(Mapper, Value, List)
    when is_function(Mapper),
         is_list(List) ->
    length([1 || Item <- List, Mapper(Item) == Value]).


split_by(Input, Divider) ->
    binary:split(Input, Divider, [global, trim]).

isolated(Function) ->
    isolated(Function, 1000).

isolated(Function, Timeout) when is_function(Function) ->
    isolated(false, Function, Timeout);

isolated(true, Function) ->
    isolated(true, Function, 1000);

isolated(false, Function) -> Function().

isolated(false, Function, _Timeout) -> Function();

isolated(true, Function, Timeout) when is_function(Function) ->
    Self = self(),
    spawn(fun () -> isolated_runner(Self, Function) end),
    receive
        {ok, Response} -> Response;
        {throw, EValue} -> erlang:throw(EValue);
        {error, EValue} -> erlang:error(EValue)
    after Timeout -> erlang:error(timeout)
    end.

isolated_runner(From, Function) when is_function(Function) ->
    try Function() of
        Value -> From ! {ok, Value}
    catch
        EType:EValue -> From ! {EType, EValue}
    end.

%% Cite: https://www.programming-idioms.org/idiom/10/shuffle-a-list/1005/erlang
shuffle(List) when is_list(List) ->
    [Element || {_RandIndex, Element} <- lists:sort([{rand:uniform(), N} || N <- List])].
