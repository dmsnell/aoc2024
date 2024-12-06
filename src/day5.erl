-module(day5).
-moduledoc """
Started at 10:44pm
Part 1 solved at 11:36pm
Part 2 solved at 11:59pm
Solved in 1h 15m.

### Optimization

 - Build a tree for part 2 for ordering, where insertion
   would be a more direct route, and then traverse the
   tree once built to get ordering.

 - Replace the default `map()` implementation for the ordering
   rules with a custom lookup three.

     - This optimization led to a 2–2.5x speedup.

#### Failed optimizations

 - After getting the problem working I tried to replace
   the `map` form of `Rejects` with a list, thinking that
   a small list membership test would be faster than hash
   lookup, but this turned out to be wrong and the run of
   both parts was ever so slightly slower with the list.

 - Stop parsing the numbers; rely on the string inputs
   until forming the sum at the end. There’s no need to
   parse them into integers and the hash them when a
   string hash would be fine from the start.

     - This turned out to be _considerably_ slower than
       the intitial solution — 5–6x slower. Not sure why.
       Perhaps the numeric hashing is faster.


@author Dennis Snell <dmsnell@xkq.io>
@copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
Created : 04. Dec 2024 10:42 PM
""".
-author("Dennis Snell <dmsnell@xkq.io>").

%% API
-export([config/0]).

config() -> #{
    p1 => {fun p1_tree_lookup/1, groups_and_lines},
    p2 => {fun p2_tree_lookup/1, groups_and_lines}
}.


p1_tree_lookup([OrderingRules, Updates]) ->
    Orderings = parse_ordering_rules(OrderingRules),
    lists:foldl(
        fun
            (<<>>, Sum) ->
                Sum;

            (Update, Sum) ->
                NumGroups = binary:split(Update, <<",">>, [global, trim]),
                Pages = [{H - $0, L - $0} || <<H, L>> <- NumGroups],
                case is_right_order(Orderings, Pages) of
                    true  -> Sum + to_num(list_mid(Pages));
                    false -> Sum
                end
        end,
        0,
        Updates
    ).


to_num({H, L}) -> 10 * H + L.


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
    case rl_get(Orderings, Page, Prior) of
        safe   -> can_follow(Orderings, Page, Priors);
        reject -> false
    end.


rl_leaf_new() ->
    {safe, safe, safe, safe, safe, safe, safe, safe, safe, safe}.

rl_new() ->
    {rl_leaf_new(), rl_leaf_new(), rl_leaf_new(), rl_leaf_new(), rl_leaf_new(), rl_leaf_new(), rl_leaf_new(), rl_leaf_new(), rl_leaf_new(), rl_leaf_new()}.

rl_add(RL, {H, L} = _Page, {PH, PL} = _Prior) ->
    PageHigh = element(H + 1, RL),
    PageLow  = case element(L + 1, PageHigh) of
        safe  -> rl_new();
        Level -> Level
    end,
    PriorHigh = case element(PH + 1, PageLow) of
        safe -> rl_leaf_new();
        Leaf -> Leaf
    end,
    setelement(H + 1, RL, setelement(
        L + 1, PageHigh, setelement(
            PH + 1, PageLow, setelement(
                PL + 1, PriorHigh, reject
            )
        )
    ) ).

rl_has(RL, {H, L} = _Page) ->
    case element(L + 1, element(H + 1, RL)) of
        safe -> false;
        _    -> true
    end.

rl_get(RL, {H, L} = _Page, {PH, PL} = _Prior) ->
    PageHigh = element(H + 1, RL),
    PageLow  = case element(L + 1, PageHigh) of
        safe  -> rl_new();
        Level -> Level
    end,
    PriorHigh = case element(PH + 1, PageLow) of
        safe -> rl_leaf_new();
        Leaf -> Leaf
    end,
    element(PL + 1, PriorHigh).


parse_ordering_rules(Lines) ->
    parse_ordering_rules(Lines, rl_new()).

parse_ordering_rules([], Orderings) ->
    Orderings;

parse_ordering_rules([Line | Lines], Orderings) ->
    {Prior, Page} = parse_ordering_line(Line),
    NextOrderings = rl_add(Orderings, Page, Prior),
    parse_ordering_rules(Lines, NextOrderings).

parse_ordering_line(<<PH, PL, "|", H, L>>) ->
    {{H - $0, L - $0}, {PH - $0, PL - $0}}.

p2_tree_lookup([OrderingRules, Updates]) ->
    Orderings = parse_ordering_rules(OrderingRules),
    lists:foldl(
        fun
            (<<>>, Sum) ->
                Sum;

            (Update, Sum) ->
                NumGroups = binary:split(Update, <<",">>, [global, trim]),
                Pages = [{H - $0, L - $0} || <<H, L>> <- NumGroups],
                case is_right_order(Orderings, Pages) of
                    true  -> Sum;
                    false ->
                        {H, L} = list_mid(in_order(Orderings, Pages)),
                        Sum + H * 10 + L
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
    case rl_has(Orderings, Page) of
        false -> Ordered ++ [Page];
        true  -> insert(Orderings, Page, [], Ordered)
    end.

insert(_Orderings, Page, Prefix, []) ->
    Prefix ++ [Page];

insert(Orderings, Page, Prefix, [Next | Tail]) ->
    case rl_get(Orderings, Page, Next) of
        safe   -> insert(Orderings, Page, Prefix ++ [Next], Tail);
        reject -> Prefix ++ [Page, Next | Tail]
    end.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(143, aoc:test(day5, p1, "day5_a")).

p2_test() ->
    ?assertEqual(123, aoc:test(day5, p2, "day5_a")).

-endif.
