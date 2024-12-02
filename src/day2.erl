-module(day2).
-moduledoc """
Completed in one hour.

@author Dennis Snell <dmsnell@xkq.io>
@copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
Created : 01. Dec 2024 9:32 PM
""".
-author("Dennis Snell <dmsnell@xkq.io>").
-behavior(aoc).

%% API
-export([config/0]).

config() -> #{
    p1 => {fun p1_submitted/1, lines},
    p2 => {fun p2_submitted/1, lines}
}.

-doc """
Submitted solution

#### Benchmark

```
    #{total => {2851.713,ms},
      answer => 326,
      measured => {avg,{0.205,ms},min,{0.13,ms}},
      total_per => {0.285,ms}}
```
""".
p1_submitted(Lines) ->
    lists:foldl(
        fun
            (<<>>, Count) ->
                Count;

            (Line, Count) ->
                case is_safe(Line) of
                    true  -> Count + 1;
                    false -> Count
                end
        end,
        0,
        Lines
    ).

is_safe(Line) ->
    is_safe(first, Line).

is_safe(first, Line) ->
    {{Prev, _Length}, Rest} = day1:int(Line, 0, 0),
    is_safe({second, Prev}, Rest);

is_safe(_, <<>>) ->
    true;
is_safe({second, Prev}, Buffer) ->
    case day1:int(Buffer, 0, 0) of
        {{Next, _Length}, Rest} when Next >= Prev + 1, Next =< Prev + 3->
            is_safe({increasing, Next}, Rest);

        {{Next, _Length}, Rest} when Next =< Prev - 1, Next >= Prev - 3 ->
            is_safe({decreasing, Next}, Rest);

        _ ->
            false
    end;

is_safe({increasing, Prev}, Buffer) ->
    case day1:int(Buffer, 0, 0) of
        {{Next, _Length}, Rest} when Next >= Prev + 1, Next =< Prev + 3 ->
            is_safe({increasing, Next}, Rest);

        _ ->
            false
    end;

is_safe({decreasing, Prev}, Buffer) ->
    case day1:int(Buffer, 0, 0) of
        {{Next, _Length}, Rest} when Next =< Prev - 1, Next >= Prev - 3 ->
            is_safe({decreasing, Next}, Rest);

        _ ->
            false
    end.


-doc """
Submitted solution

Initially I tried to add a new `MaxInvalid` parameter to part 1’s solution,
but then after an initial change led to a wrong result, I realized it wasn’t
worth injecting that customizable behavior when only one number could ever
be omitted. Instead, I created a very naive function to loop through the
numbers and remove one, attempting to see if the new list is unsafe.

An optimization would look ahead and see which number could be rejected to
continue testing. I started this way, but the function arguments weren’t
designed to convey this well, and I didn’t pause to think how to decide
which number to drop. This would allow a single-pass through the line,
however, saving potentially a lot of time.

#### Benchmark

```
#{total => {32500.186,ms},
  answer => 381,
  measured => {avg,{3.156,ms},min,{2.638,ms}},
  total_per => {3.25,ms}}
```
""".
p2_submitted(Lines) ->
    lists:foldl(
        fun
            (<<>>, Count) ->
                Count;

            (Line, Count) ->
                case is_safe(Line) of
                    true  -> Count + 1;
                    false ->
                        case is_safe_removing_one(Line) of
                            true  -> Count + 1;
                            false -> Count
                        end
                end
        end,
        0,
        Lines
    ).

is_safe_removing_one(Line) ->
    Numbers = string:split(Line, <<" ">>, all),
    is_safe_removing_one(loop, Numbers, 1, length(Numbers)).

is_safe_removing_one(loop, _Numbers, N, Max) when N > Max->
    false;

is_safe_removing_one(loop, Numbers, N, Max) ->
    WithoutNth = unicode:characters_to_binary(lists:join(<<" ">>, drop_nth(Numbers, N))),
    case is_safe(WithoutNth) of
        true  -> true;
        false -> is_safe_removing_one(loop, Numbers, N + 1, Max)
    end.


drop_nth(List, N) ->
    {Head, [_Dropped | Tail]} = lists:split(N - 1, List),
    Head ++ Tail.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(2, aoc:test(day2, p1, "day2_a")).

p2_test() ->
    ?assertEqual(4, aoc:test(day2, p2, "day2_a")).

-endif.
