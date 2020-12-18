%% @author Timofey Tipishev <tipishev@gmail.com>
%% @doc This is AOC2020 <em>day11</em> solution
%% @reference <a href="https://adventofcode.com/2020/day/11">AOC 2020 day 11</a> for
%% @since 2020-12-18
%% @version 0.5.0

-module(day11).

-export([solve_part1/1, solve_part2/1]).

% could be shielded with -ifdef(TEST) macro
-export([parse/1, life/1, adjacent/2]).

%%% solution

solve_part1(_Input) ->
    undefined.

solve_part2(_Input) ->
    undefined.

life(GridStr) ->
    parse(GridStr).

-type tile() :: floor | seat | occupied.
-type grid() :: [[tile(), ...], ...].
-spec parse(GridStr) -> Grid when
      GridStr :: string(),
      Grid :: grid().
%% @doc Convert Grid string to Grid.
%% @param GridStr string in the format of task input.
%% @returns is a double-list of atoms floor, seat,
%% @throws shadows.
%% and occupied.

parse(GridStr) ->
    Lines = string:lexemes(GridStr, "\n"),
    [parse_line(Line) || Line <- Lines].

parse_line(Line) ->
    [parse_symbol(Symbol) || Symbol <- Line].

parse_symbol($.) -> floor;
parse_symbol($L) -> seat;
parse_symbol($#) -> occupied.

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
