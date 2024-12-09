-module(day9).
-moduledoc """
Started at 10:00pm
Part 1 solved at 11:01pm
Part 2 solved at 1:04am
Solved in 3h 4m.

### Optimization

 - Probably don’t need to remove from reversed list of files.
   Instead store an offset at which the files are copied or moved.
 - Can probably iterate over the files instead of tracking the free list.
   This might save expensive list operations.
 - Combine FS compaction and hash building to eliminate the intermediate list.

@author Dennis Snell <dmsnell@xkq.io>
@copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
Created : 08. Dec 2024 9:42 PM
""".
-author("Dennis Snell <dmsnell@xkq.io>").

%% API
-export([config/0]).

config() -> #{
    p1 => {fun p1_submitted/1, raw},
    p2 => {fun p2_submitted/1, raw}
}.

-type span() :: {At :: non_neg_integer(), Length :: non_neg_integer()}.
-type file() :: {ID :: non_neg_integer(), span()}.

-record(fs, {
    size  = 0      :: non_neg_integer(),
    files = []     :: list(file()),
    rev_files = [] :: list(file()),
    free  = []     :: list(non_neg_integer())
}).


-doc """
#### Benchmark

```
#{total => {4631.92,ms},
  answer => 6385338159127,
  measured => {avg,{926.192,ms},min,{921.086,ms}},
  total_per => {926.384,ms}}
```
""".
p1_submitted(Buffer) ->
    InitialFS = read_table(Buffer),
    Compacted = compact(InitialFS),
    hash(Compacted).


read_table(Buffer) ->
    read_table(#fs{}, next_file, Buffer, 0, 0).

read_table(#fs{files = Files} = FS, _State, <<>>, N, _ID) ->
    FS#fs{size = N, files = lists:reverse(Files), rev_files = Files};

read_table(#fs{} = FS, State, <<"\n">>, N, ID) ->
    read_table(FS, State, <<>>, N, ID);

read_table(#fs{files = Files} = FS, next_file, <<D, Buffer/binary>>, N, ID) when D >= $0, D =< $9 ->
    Stride = D - $0,
    read_table(FS#fs{files = [{ID, {N, Stride}} | Files]}, next_free, Buffer, N + Stride, ID + 1);

read_table(#fs{free = Free} = FS, next_free, <<D, Buffer/binary>>, N, ID) when D >= $0, D =< $9 ->
    Stride = D - $0,
    read_table(FS#fs{free = Free ++ lists:seq(N, N + Stride - 1)}, next_file, Buffer, N + Stride, ID).


compact(#fs{} = FS) ->
    lists:flatten(compact(FS, 0, [])).

compact(#fs{
    files = [{ID, _} | _],
    rev_files = [{ID, _} | _] = RevFiles
}, _At, Blocks) ->
    [Blocks | [lists:duplicate(L, F_ID) || {F_ID, {_, L}} <- lists:reverse(RevFiles), F_ID >= ID]];

compact(#fs{files = [{ID, {At, FileSize}} | Files]} = FS, At, Blocks) ->
    compact(FS#fs{files = Files}, At + FileSize, [Blocks | lists:duplicate(FileSize, ID)]);

compact(#fs{
    free = Free,
    files = [{_ID, {NextAt, _}} | _],
    rev_files = [{ID, {LastAt, LastSize}} | RevFiles]
} = FS, At, Blocks) ->
    % There are NextAt - At blocks free; start filling in the from the last file.
    ToUse = min(LastSize, NextAt - At),
    {_UsedFree, RemainingFree} = lists:splitwith(fun (V) -> V < NextAt end, Free),
    RemainingFiles = if
        ToUse == LastSize -> RevFiles;
        true              -> [{ID, {LastAt, LastSize - ToUse}} | RevFiles]
    end,
    compact(FS#fs{
        free      = RemainingFree,
        rev_files = RemainingFiles
    }, At + ToUse, [Blocks | lists:duplicate(ToUse, ID)]).


hash(Blocks) ->
    hash(Blocks, 0, 0).

hash([], _Position, Hash) ->
    Hash;

hash([ID | Blocks], Position, Hash) ->
    hash(Blocks, Position + 1, Hash + ID * Position).


-doc """
#### Benchmark

```
#{total => {30979.678,ms},
  answer => 6415163624282,
  measured => {avg,{6195.767,ms},min,{6038.112,ms}},
  total_per => {6195.936,ms}}
```
""".
p2_submitted(Buffer) ->
    InitialFS = read_table(Buffer),
    Compacted = compact_contiguous(InitialFS),
    hash(Compacted).


compact_contiguous(#fs{} = FS) ->
    compact_contiguous(FS, 0, []).

compact_contiguous(#fs{files = []}, _At, Blocks) ->
    Sorted = lists:sort(fun ({A, _}, {B, _}) -> A =< B end, lists:flatten(Blocks)),
    {_EndAt, Chunks} = lists:foldl(
        fun ({FileAt, FileBlocks}, {At, FS}) ->
            PrefixLength = FileAt - At,
            Prefix = lists:duplicate(PrefixLength, 0),
            {FileAt + length(FileBlocks), [FS | [Prefix | FileBlocks]]}
        end,
        {0, []},
        Sorted
    ),
    lists:flatten(Chunks);

compact_contiguous(#fs{
    files = [{ID, {At, Size}} | Files],
    rev_files = RevFiles
} = FS, At, Blocks) ->
    compact_contiguous(
        FS#fs{
            files = Files,
            rev_files = remove_file(RevFiles, ID)
        },
        At + Size,
        [Blocks | [{At, lists:duplicate(Size, ID)}]]
    );

compact_contiguous(#fs{
    free = Free,
    files = Files,
    rev_files = [{ID, {RevAt, Length}} | RevFiles]
} = FS, At, Blocks) ->
    % Is there a space big enough to fit this file?
    case first_free_span(RevAt, Free, Length) of
        no_space ->
            compact_contiguous(FS#fs{rev_files = RevFiles}, At, Blocks);

        {free, FreeAt, RemainingFree} ->
            RemainingFiles = remove_file(Files, ID),
            compact_contiguous(
                FS#fs{
                    free = lists:sort(RemainingFree ++ lists:seq(RevAt, RevAt + Length)),
                    files = RemainingFiles,
                    rev_files = RevFiles
                },
                if FreeAt == At -> FreeAt + Length; true -> At end,
                [Blocks | [{FreeAt, lists:duplicate(Length, ID)}]]
            )
    end;

compact_contiguous(#fs{} = FS, At, Blocks) ->
    compact_contiguous(FS, At + 1, Blocks).


first_free_span(_Before, [], _Length) ->
    no_space;

first_free_span(Before, [Next | _Free], _Length) when Next >= Before ->
    no_space;

first_free_span(Before, [Next | Free], Length) ->
    first_free_span(Before, Free, Length, Next, Next, 1, [Next], []).

first_free_span(_Before, [], _Length, _Start, _Prev, _L, _Used, _Skipped) ->
    no_space;

first_free_span(_Before, Free, Length, Start, _Prev, Length, _Used, Skipped) ->
    {free, Start, lists:reverse(Skipped) ++ Free};

first_free_span(Before, [Next | _Free], _Length, _Start, _Prev, _L, _Used, _Skipped) when Next >= Before ->
    no_space;

first_free_span(Before, [Next | Free], Length, Start, Prev, L, Used, Skipped) when Next == Prev + 1 ->
    first_free_span(Before, Free, Length, Start, Next, L + 1, [Next | Used], Skipped);

first_free_span(Before, [Next | Free], Length, _Start, _Prev, _L, Used, Skipped) ->
    first_free_span(Before, Free, Length, Next, Next, 1, [Next], lists:reverse(Used) ++ Skipped).


remove_file(Files, ID) ->
    remove_file(Files, ID, []).

remove_file([], _ID, Reversed) ->
    lists:reverse(Reversed);

remove_file([{ID, _} | Files], ID, Reversed) ->
    lists:reverse(Reversed) ++ Files;

remove_file([File | Files], ID, Reversed) ->
    remove_file(Files, ID, [File | Reversed]).


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(1928, aoc:test(day9, p1, "day9_a")).

p2_test() ->
    ?assertEqual(2858, aoc:test(day9, p2, "day9_a")).

-endif.
