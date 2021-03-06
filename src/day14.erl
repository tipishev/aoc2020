-module(day14).

-export([solve_part1/1, solve_part2/1]).

% for testing part 1
-export([parse/1, bin36/1, mask_value/2, dock/1, map_sum/1]).

% for testing part 2
-export([dock2/1, mask_address/2, expand/1]).

%%% solution

solve_part1(Input) ->
     day14:map_sum(day14:dock(day14:parse(Input))).

solve_part2(Input) ->
     day14:map_sum(day14:dock2(day14:parse(Input))).

%%% Parse

parse(Input) ->
    Lines = split(Input, "\n"),
    [parse(line, Line) || Line <- Lines].

parse(line, Line) ->
    [Command | Args] = split(Line, " =[]"),
    if
        Command =:= "mask" ->
           [Mask] = Args,
           {mask, Mask};
        Command =:= "mem" ->
            [Address, Value] = [str_to_int(Arg) || Arg <- Args],
            {mem, Address, Value}
    end.

%%% Part 1 Solution

dock(Program) ->
    dock(Program, #{}, undefined).

dock([], Addresses, _Mask) -> Addresses;
dock([{mask, NewMask} | CommandsTail], Addresses, _Mask) ->
    dock(CommandsTail, Addresses, NewMask);
dock([{mem, Address, Value} | CommandsTail], Addresses, Mask) ->
    dock(CommandsTail,
         Addresses#{Address => bin_to_int(mask_value(bin36(Value), Mask))},
         Mask).

%%% Part 2 Solution

dock2(Program) ->
    dock2(Program, #{}, undefined).

dock2([], Addresses, _Mask) -> Addresses;
dock2([{mask, NewMask} | CommandsTail], Addresses, _Mask) ->
    dock2(CommandsTail, Addresses, NewMask);
dock2([{mem, Address, Value} | CommandsTail], Addresses, Mask) ->
    MaskedAddress = mask_address(bin36(Address), Mask),
    ExpandedAddresses = expand(MaskedAddress),
    NewAddresses = lists:foldl(
                     fun(Elem, Acc) ->
                             Acc#{bin_to_int(Elem) => Value}
                     end,
                     Addresses, ExpandedAddresses),
    dock2(CommandsTail, NewAddresses, Mask).

%% @doc Applies bitmask Mask to Value.
mask_value(Value, Mask) ->
    mask_value(Value, Mask, []).

mask_value([], [], Acc) ->
    lists:reverse(Acc);
mask_value([ValueHead | ValueTail],
     [$X | MaskTail], Acc) ->
    mask_value(ValueTail, MaskTail, [ValueHead | Acc]);
mask_value([_ | ValueTail],
     [MaskHead | MaskTail], Acc) ->
    mask_value(ValueTail, MaskTail, [MaskHead | Acc]).

%% @doc Applies bitmask Mask to Address.
mask_address(Address, Mask) ->
    mask_address(Address, Mask, []).

mask_address([], [], Acc) ->
    lists:reverse(Acc);
mask_address([_ValueHead | ValueTail], [$X | MaskTail], Acc) ->
    mask_address(ValueTail, MaskTail, [$X | Acc]);
mask_address([ValueHead | ValueTail], [$0 | MaskTail], Acc) ->
    mask_address(ValueTail, MaskTail, [ValueHead | Acc]);
mask_address([_ValueHead | ValueTail], [$1 | MaskTail], Acc) ->
    mask_address(ValueTail, MaskTail, [$1 | Acc]).

%% @doc Expands a floating value to a list of concrete values.
expand(Floating) ->
    DeepList = expand([], Floating),
    LongString = lists:flatten(DeepList),
    chunks(LongString, length(Floating)).

expand(ReversedConcrete, []) ->
    lists:reverse(ReversedConcrete);
expand(Suffix, [$X | FloatingTail]) ->
    [expand([$0 | Suffix], FloatingTail),
     expand([$1 | Suffix], FloatingTail)];
expand(Suffix, [Any | FloatingTail]) ->
    expand([Any | Suffix], FloatingTail).

chunks(List, ChunkSize) ->
    chunks(List, ChunkSize, []).

chunks([], _ChunkSize, Acc) ->
    lists:reverse(Acc);
chunks(List, ChunkSize, Acc) ->
    {Chunk, Tail} = lists:split(ChunkSize, List),
    chunks(Tail, ChunkSize, [Chunk | Acc]).



%% @doc Sums values of the map
map_sum(Map) ->
    lists:sum(maps:values(Map)).

%%% Herlpers
split(Str, Sep) -> string:lexemes(Str, Sep).
str_to_int(Str) -> list_to_integer(Str).
bin36(N) ->
    BinStr = integer_to_list(N, 2),
    Padded = string:pad(BinStr, 36, leading, "0"),
    lists:flatten(Padded).
bin_to_int(BinStr) -> list_to_integer(BinStr, 2).
