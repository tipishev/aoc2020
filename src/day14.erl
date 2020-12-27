-module(day14).

-export([solve_part1/1, solve_part2/1]).

-export([parse/1]).

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


%%% Herlpers
split(Str, Sep) -> string:lexemes(Str, Sep).
to_int(Str) -> list_to_integer(Str).

