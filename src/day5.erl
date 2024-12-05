-module(day5).
-moduledoc """
Started at 10:44pm
Part 1 solved at 11:36pm
Part 2 solved at 11:59pm
Solved in 1h 15m.

### Optimization

 - Stop parsing the numbers; rely on the string inputs
   until forming the sum at the end. There’s no need to
   parse them into integers and the hash them when a
   string hash would be fine from the start.

 - Build a tree for part 2 for ordering, where insertion
   would be a more direct route, and then traverse the
   tree once built to get ordering.

#### Failed optimizations

 - After getting the problem working I tried to replace
   the `map` form of `Rejects` with a list, thinking that
   a small list membership test would be faster than hash
   lookup, but this turned out to be wrong and the run of
   both parts was ever so slightly slower with the list.

@author Dennis Snell <dmsnell@xkq.io>
@copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
Created : 04. Dec 2024 10:42 PM
""".
-author("Dennis Snell <dmsnell@xkq.io>").

%% API
-export([config/0]).

config() -> #{
    p1 => {fun p1_submitted/1, groups_and_lines},
    p2 => {fun p2_submitted/1, groups_and_lines}
}.


p1_submitted([OrderingRules, Updates]) ->
    Orderings = parse_ordering_rules(OrderingRules),
    lists:foldl(
        fun
            (<<>>, Sum) ->
                Sum;

            (Update, Sum) ->
                NumGroups = binary:split(Update, <<",">>, [global, trim]),
                Pages = [begin {{N, _}, <<>>} = day1:int(PageNum, 0, 0), N end || PageNum <- NumGroups],
                case is_right_order(Orderings, Pages) of
                    true  -> Sum + list_mid(Pages);
                    false -> Sum
                end
        end,
        0,
        Updates
    ).


list_mid(List) ->
    list_mid(List, List).

list_mid([Half | _], []) ->
    Half;
list_mid([Half | _], [_]) ->
    Half;
list_mid([_| HalfList], [_, _ | FullList]) ->
    list_mid(HalfList, FullList).


is_right_order(Orderings, Pages) ->
    is_right_order(Orderings, Pages, []).

is_right_order(_Orderings, [], _Priors) ->
    true;

is_right_order(Orderings, [Page | Pages], Priors) ->
    case can_follow(Orderings, Page, Priors) of
        true  -> is_right_order(Orderings, Pages, [Page | Priors]);
        false -> false
    end.


can_follow(_Orderings, _Page, []) ->
    true;
can_follow(Orderings, Page, [Prior | Priors]) ->
    case maps:get(Prior, Orderings, not_found) of
        not_found ->
            can_follow(Orderings, Page, Priors);

        Rejects when is_map(Rejects) ->
            case maps:is_key(Page, Rejects) of
                true  -> false;
                false -> can_follow(Orderings, Page, Priors)
            end
    end.


parse_ordering_rules(Lines) ->
    parse_ordering_rules(Lines, #{}).

parse_ordering_rules([], Orderings) ->
    Orderings;

parse_ordering_rules([Line | Lines], Orderings) ->
    {{Earlier, _}, <<"|", Rest/binary>>} = day1:int(Line, 0, 0),
    {{Later, _}, <<>>} = day1:int(Rest, 0, 0),
    NextOrderings = maps:update_with(
        Later,
        fun (Priors) -> maps:put(Earlier, true, Priors) end,
        #{Earlier => true},
        Orderings
    ),
    parse_ordering_rules(Lines, NextOrderings).

p2_submitted([OrderingRules, Updates]) ->
    Orderings = parse_ordering_rules(OrderingRules),
    lists:foldl(
        fun
            (<<>>, Sum) ->
                Sum;

            (Update, Sum) ->
                NumGroups = binary:split(Update, <<",">>, [global, trim]),
                Pages = [begin {{N, _}, <<>>} = day1:int(PageNum, 0, 0), N end || PageNum <- NumGroups],
                case is_right_order(Orderings, Pages) of
                    true  -> Sum;
                    false -> Sum + list_mid(in_order(Orderings, Pages))
                end
        end,
        0,
        Updates
    ).


in_order(Orderings, Pages) ->
    in_order(Orderings, Pages, []).

in_order(_Orderings, [], Ordered) ->
    Ordered;

in_order(Orderings, [Page | Pages], PrevOrdered) ->
    in_order(Orderings, Pages, insert(Orderings, Page, PrevOrdered)).


insert(_Orderings, Page, []) ->
    [Page];


insert(Orderings, Page, Ordered) ->
    case maps:get(Page, Orderings, not_found) of
        not_found ->
            Ordered ++ [Page];

        Rejects when is_map(Rejects) ->
            insert(Orderings, Page, Rejects, [], Ordered)
    end.

insert(_Orderings, Page, _Rejects, Prefix, []) ->
    Prefix ++ [Page];

insert(Orderings, Page, Rejects, Prefix, [Next | Tail]) ->
    case maps:get(Next, Rejects, not_found) of
        not_found -> insert(Orderings, Page, Rejects, Prefix ++ [Next], Tail);
        true      -> Prefix ++ [Page, Next | Tail]
    end.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(143, aoc:test(day5, p1, "day5_a")).

p2_test() ->
    ?assertEqual(123, aoc:test(day5, p2, "day5_a")).

-endif.
