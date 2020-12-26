-module(day13).

-export([solve_part1/1, solve_part2/1]).

-export([
         parse/1, earliest/1,
         parse_congruences/1, extended_gcd/2, solve_congruences/1
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

% FIXME jump-start recursion, deduplicate
solve_congruences([ {n, N1, a, A1}, {n, N2, a, A2} | T]) ->
    {M1, M2} = bezout(N1, N2),
    N = N1 * N2,
    A = A1 * M2 * N2 + A2 * M1 * N1,
    NonNegA = case A < 0 of
                  true -> A + N;
                  false -> A
              end,
    lists:foldl(fun solve_congruences_folder/2,
                                {n, N, a, NonNegA}, T).

solve_congruences_folder(_Elem={n, N1, a, A1},
                         _Acc={n, N2, a, A2}) ->
    {M1, M2} = bezout(N1, N2),
    N = N1 * N2,
    A = A1 * M2 * N2 + A2 * M1 * N1,
    NonNegA = case A < 0 of
                  true -> A + N;
                  false -> A rem N
              end,
    {n, N, a, NonNegA}.


bezout(A, B) ->
    {m1, M1, m2, M2, _, _, _, _, _, _} = extended_gcd(A, B),
    {M1, M2}.


%%% @doc Computes Greatest Common Divisor and Bezout's coefficients.
%%% @reference wikipedia.org/wiki/Extended_Euclidean_algorithm
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
