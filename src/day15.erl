-module(day15).

-export([solve_part1/1, solve_part2/1]).

-export([parse/1]).

%%% solution

solve_part1(_Input) ->
    undefined.

solve_part2(_Input) ->
    undefined.


%%% Parse
parse(Input) ->
    [list_to_integer(N) || N <- string:lexemes(Input, ",")].
