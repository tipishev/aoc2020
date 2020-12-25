-module(day13).

-export([solve_part1/1, solve_part2/1]).

-export([parse/1]).

%%% Solution

solve_part1(_Input) ->
    undefined.

solve_part2(_Input) ->
    undefined.

%%% Parse

parse(Input) ->
    [EarliestStr, BusesStr] = split(Input, "\n"),
    Earliest = to_int(EarliestStr),
    Buses = [to_int(BusNumber)
             || BusNumber <- split(BusesStr, ","),
                BusNumber =/= "x" ],
    {earliest, Earliest, buses, Buses}.

%%% Herlpers
split(Str, Sep) -> string:lexemes(Str, Sep).
to_int(Str) -> list_to_integer(Str).
