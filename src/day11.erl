-module(day11).

-export([solve_part1/1, solve_part2/1]).

% could be shielded with IFDEF(?TEST) macro
-export([life/1, adjacent/2]).

% -define(MAX_X, 10).
% -define(MAX_Y, 10).

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

%% adjacent coordinates, sorted
adjacent({MaxX, MaxY}, {X, Y}) ->
    lists:sort([
     {AdjX, AdjY} ||
     AdjX <- [X - 1, X, X + 1],
     AdjY <- [Y - 1, Y, Y + 1],
     {AdjX, AdjY} =/= {X, Y},  % self is not adjacent
     AdjX > 0, AdjY > 0,
     AdjX =< MaxX, AdjY =< MaxY
    ]).
