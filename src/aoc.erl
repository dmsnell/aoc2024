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
    print/2,
    print/3,
    print_all/0,
    print_all/1,
    test/3
]).

-type input_type() :: raw | groups_and_lines | lines | number_list.
-type runtime_config() :: {
    fun((Input :: term()) -> term()),
    input_type()
}.

-callback config() -> #{atom() => runtime_config()}.

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
    {Day, Part, Answer, _Time} = solve(Day, Part),
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
        answer    => Answer,
        measured  => Measured,
        total     => ms(Total / 1000),
        total_per => ms(Total / Iterations / 1000)
    }.

ms(MS) ->
    {round(MS * 1000) / 1000, ms}.


solve(Day, Part) ->
    solve(Day, Part, atom_to_list(Day)).

solve(Day, Part, InputName) ->
    #{Part := {Fun, InputType}} = Day:config(),
    utils:isolated(fun () ->
        {USecs, Value} = timer:tc(Fun, [parse_input(Day, get_input(InputName, InputType))]),
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
    Config = Day:config(),
    [{Day, Part} || Part <- maps:keys(Config), Part == p1 orelse Part == p2].

parse_input(Day, Input) ->
    case erlang:function_exported(Day, parse_input, 1) of
        true  -> Day:parse_input(Input);
        false -> Input
    end.

print(Day, Part) ->
    print(Day, Part, 10).

print(Day, Part, N) ->
    Results = sloppy_benchmark(Day, Part, N),
    #{
        answer    := A,
        total     := {TV, TU},
        measured  := {avg, {AV, AU}, min, {MV, MU}},
        total_per := {TPV, TPU}
    } = Results,
    io:format("\e[34m~p~p\e[90m (\e[36m~p\e[90m) took \e[33m~p \e[2m~p\e[m\n", [Day, Part, A, TV, TU]),
    io:format("\e[90m ├ avg = \e[33m~p \e[2m~p\e[m\n", [AV, AU]),
    io:format("\e[90m ├ min = \e[33m~p \e[2m~p\e[m\n", [MV, MU]),
    io:format("\e[90m └ total per = \e[33m~p \e[2m~p\e[m\n\n", [TPV, TPU]),
    {Day, Part, Results}.


print_all() ->
    print_all(10).

print_all(N) ->
    Problems = lists:flatten([get_problems(Day) || Day <- get_days()]),
    [sloppy_benchmark(Day, Part, N) || {Day, Part} <- lists:sort(fun problem_sort/2, Problems)],
    ok.

test(Day, Part, InputName) ->
    #{Part := {Fun, InputType}} = Day:config(),
    utils:isolated(fun () ->
        {_USecs, Value} = timer:tc(Fun, [parse_input(Day, get_input(InputName, InputType))]),
        Value
   end, infinity).
