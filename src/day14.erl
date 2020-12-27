-module(day14).

-export([solve_part1/1, solve_part2/1]).

-export([parse/1, bin36/1, mask/2]).

%%% solution

solve_part1(_Input) ->
    undefined.

solve_part2(_Input) ->
    undefined.

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
            [Address, Value] = [to_int(Arg) || Arg <- Args],
            {mem, Address, Value}
    end.

%%% Part 1 Solution

mask(Value, Mask) ->
    mask(Value, Mask, []).

mask([], [], Acc) ->
    lists:reverse(Acc);
mask([ValueHead | ValueTail],
     [$X | MaskTail], Acc) ->
    mask(ValueTail, MaskTail, [ValueHead | Acc]);
mask([_ | ValueTail],
     [MaskHead | MaskTail], Acc) ->
    mask(ValueTail, MaskTail, [MaskHead | Acc]).
    



%%% Herlpers
split(Str, Sep) -> string:lexemes(Str, Sep).
to_int(Str) -> list_to_integer(Str).
bin36(N) ->
    BinStr = integer_to_list(N, 2),
    Padded = string:pad(BinStr, 36, leading, "0"),
    lists:flatten(Padded).

