%%%-------------------------------------------------------------------
%%% @author Dennis Snell <dmsnell@xkq.io>
%%% @copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
%%% @doc
%%%
%%% @end
%%% Created : 21. Nov 2024 3:32 PM
%%%-------------------------------------------------------------------
-module(aoc).
-author("dmsnell").

%% API

-export([
    sloppy_benchmark/2,
    sloppy_benchmark/3,
    solve/2,
    solve/3,
    solve_all/0,
    print_all/0
]).

-type input_type() :: raw | groups_and_lines | lines | number_list.

-callback input_type(atom()) -> input_type().
-callback parse_input(term()) -> term().
-callback p1(term()) -> term().
-callback p2(term()) -> term().
-optional_callbacks([parse_input/1, p2/1]).

solve_all() ->
    Problems = lists:flatten([get_problems(Day) || Day <- get_days()]),
    lists:sort(fun problem_sort/2, [solve(Day, Part) || {Day, Part} <- Problems]).

problem_sort(A, B) ->
    <<"day", DayA/binary>> = atom_to_binary(element(1, A)),
    <<"day", DayB/binary>> = atom_to_binary(element(1, B)),
    <<"p", PartA/binary>> = atom_to_binary(element(2, A)),
    <<"p", PartB/binary>> = atom_to_binary(element(2, B)),
    SortA = {binary_to_integer(DayA), binary_to_integer(PartA)},
    SortB = {binary_to_integer(DayB), binary_to_integer(PartB)},
    SortA < SortB.


sloppy_benchmark(Day, Part) ->
    sloppy_benchmark(Day, Part, 200).

sloppy_benchmark(Day, Part, Iterations) ->
    WarmupIterations = 25,
    _Warmup = lists:foreach(
        fun (_) -> solve(Day, Part) end,
        lists:seq(1, WarmupIterations)
    ),
    {Total, Measured} = timer:tc(fun () ->
        Times = [
            begin
                {Day, Part, _Answer, {MS, ms}} = solve(Day, Part),
                MS
            end
            || _ <- lists:seq(1, Iterations)
        ],
        {avg, ms(lists:sum(Times) / Iterations), min, ms(lists:min(Times))}
    end),
    #{
        measured  => Measured,
        total     => ms(Total / 1000),
        total_per => ms(Total / Iterations / 1000)
    }.

ms(MS) ->
    {round(MS * 1000) / 1000, ms}.


solve(Day, Part) ->
    solve(Day, Part, atom_to_list(Day)).

solve(Day, Part, InputName) ->
    utils:isolated(fun () ->
        InputType = Day:input_type(Part),
        {USecs, Value} = timer:tc(Day, Part, [parse_input(Day, get_input(InputName, InputType))]),
        {Day, Part, Value, {USecs / 1000, ms}}
    end, infinity).

get_days() ->
    [list_to_atom(Module) || {[$d, $a, $y, _] = Module, _, _} <- code:all_available()].

-spec get_input(Name :: string(), lines)            -> list(binary())
      ;        (Name :: string(), groups_and_lines) -> list(list(binary()))
      ;        (Name :: string(), number_list)      -> list(number())
      ;        (Name :: string(), raw)              -> binary()
      ;        (Name :: string(), io_device)        -> {io_device, file:io_device()}.
get_input(Name, Type) ->
    [BaseName] = string:replace(Name, "_faster", ""),
    input:Type(BaseName).

get_problems(Day) ->
    code:ensure_loaded(Day),
    [{Day, Part} || Part <- [p1, p2], erlang:function_exported(Day, Part, 1) == true].

parse_input(Day, Input) ->
    case erlang:function_exported(Day, parse_input, 1) of
        true  -> Day:parse_input(Input);
        false -> Input
    end.

print_all() ->
    Solutions = solve_all(),
    Lines = [io_lib:format("~p~n", [S]) || S <- Solutions],
    io:format("~s~n", [Lines]).
