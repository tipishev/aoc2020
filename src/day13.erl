-module(day13).

-export([solve_part1/1, solve_part2/1]).

-export([
         parse/1, earliest/1,
         parse_congruences/1, extended_gcd/2
        ]).

%%% Solution

solve_part1(Input) ->
    {wait, Wait, bus, Bus} = earliest(parse(Input)),
    Wait * Bus.

solve_part2(Input) ->
    Congruences = parse_congruences(Input),
    Congruences.

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
            A = (N - Index) rem N,
            NonNegA = case A >= 0 of
                          true -> A;
                          false -> A + N
                      end,
            {Index + 1, [{n, N, a, NonNegA} | Congruences]}
    end.

%%% Part 1 solution

earliest({earliest, Earliest, buses, Buses}) ->
    lists:min([{wait, (Earliest div Bus) * Bus + Bus - Earliest,
                bus, Bus}
               || Bus <- Buses]).

%%% Part 2

%%% @doc Computes Greatest Common Divisor and Bezout's coefficients.
%%% @reference https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
extended_gcd(A, B) ->
    {OldR0, R0} = {A, B},
    {OldS0, S0} = {1, 0},
    {OldT0, T0} = {0, 1},

    {OldR, _R, OldS, S, OldT, T} = extended_gcd(OldR0, R0,
                                                OldS0, S0,
                                                OldT0, T0),
    {m1, OldS, m2, OldT, gcd, OldR, quotA, T, quotB, S}.

extended_gcd(OldR, 0, OldS, S, OldT, T) ->
    {OldR, 0, OldS, S, OldT, T};
extended_gcd(OldR, R, OldS, S, OldT, T) ->
    Quotient = OldR div R,
    extended_gcd(R, OldR - Quotient * R,
                 S, OldS - Quotient * S,
                 T, OldT - Quotient * T).


%%% Herlpers
split(Str, Sep) -> string:lexemes(Str, Sep).
to_int(Str) -> list_to_integer(Str).
