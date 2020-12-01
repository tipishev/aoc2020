-module(day1).

% -behaviour(aoc_solution).

%% aoc_solution callbacks
-export([solve_part1/1, solve_part2/1]).

%% for tests
-export([sum2020/1, sum2020_p2/1]).

%%% solution behavior

solve_part1(Expenses) ->
    {E1, E2} = sum2020(Expenses),
    E1 * E2.

solve_part2(Expenses) ->
    {E1, E2, E3} = sum2020_p2(Expenses),
    E1 * E2 * E3. 

%%% internals

-spec sum2020(Expenses) -> {First, Second} when
      Expenses :: [integer(), ...],
      First :: integer(),
      Second :: integer().

sum2020(Expenses) ->
    [Result] = [{E1, E2} || E1 <- Expenses,
                            E2 <- Expenses,
                            E1 < E2,
                            E1 + E2 =:= 2020],
    Result.

-spec sum2020_p2(Expenses) -> {First, Second, Third} when
      Expenses :: [integer(), ...],
      First :: integer(),
      Second :: integer(),
      Third :: integer().

 sum2020_p2(Expenses) ->
    [Result] = [{E1, E2, E3} || E1 <- Expenses,
                            E2 <- Expenses,
                            E3 <- Expenses,
                            E1 < E2, E2 < E3,
                            E1 + E2 + E3 =:= 2020],
    Result.
