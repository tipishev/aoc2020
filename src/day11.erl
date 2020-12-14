-module(day11).

-export([solve_part1/1, solve_part2/1]).
-export([life/1]).

%%% solution

solve_part1(_Input) ->
    undefined.

solve_part2(_Input) ->
    undefined.

life(Board) ->
    parse(Board).

parse(Board) ->
    Lines = string:lexemes(Board, "\n"),
    FirstLine = lists:nth(1, Lines),
    {length(Lines), length(FirstLine)}.
