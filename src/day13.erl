-module(day13).

-export([solve_part1/1, solve_part2/1]).

-export([
         parse/1, earliest/1,
         parse_congruences/1
        ]).

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

parse_congruences(Input) ->
    [_Earliest, BusesStr] = split(Input, "\n"),
    Buses = string:lexemes(BusesStr, ","),
    {_Index, Congruences} = lists:foldl(fun congruences_folder/2,
                                        {0, []}, Buses),
    lists:sort(Congruences).

congruences_folder(Element, {Index, Congruences}) ->
    case Element =:= "x" of
        true -> {Index + 1, Congruences};
        false ->
            N = list_to_integer(Element),
            {Index + 1, [{n, N, a, (N - Index) rem N} | Congruences]}
    end.

%%% Part 1 solution

earliest({earliest, Earliest, buses, Buses}) ->
    lists:min([{wait, (Earliest div Bus) * Bus + Bus - Earliest,
                bus, Bus}
               || Bus <- Buses]).

%%% Herlpers
split(Str, Sep) -> string:lexemes(Str, Sep).
to_int(Str) -> list_to_integer(Str).
