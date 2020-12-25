-module(day13).

-export([solve_part1/1, solve_part2/1]).

-export([parse/1, earliest/1]).

%%% Solution

solve_part1(Input) ->
    {wait, Wait, bus, Bus} = earliest(parse(Input)),
    Wait * Bus.

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

earliest({earliest, Earliest, buses, Buses}) ->
    lists:min([{wait, (Earliest div Bus) * Bus + Bus - Earliest,
                bus, Bus}
               || Bus <- Buses]).

%%% Herlpers
split(Str, Sep) -> string:lexemes(Str, Sep).
to_int(Str) -> list_to_integer(Str).
