%% @author Timofey Tipishev <tipishev@gmail.com>
%% @doc This is AOC2020 <em>day11</em> solution
%% @reference <a href="https://adventofcode.com/2020/day/11">AOC 2020 day 11</a> for
%% @since 2020-12-18
%% @version 0.5.0

-module(day11).

-export([solve_part1/1, solve_part2/1]).

% could be shielded with -ifdef(TEST) macro
-export([parse/1, life/1, adjacent/2, at/2]).

%%% solution

solve_part1(_Input) ->
    undefined.

solve_part2(_Input) ->
    undefined.

life(GridStr) ->
    parse(GridStr).

-type tile() :: floor | empty | occupied.
-type grid() :: [[tile(), ...], ...].
-spec parse(GridStr) -> Grid when
      GridStr :: string(),
      Grid :: grid().
%% @doc Convert Grid string to Grid.
%% @param GridStr string in the format of task input.
%% @returns is a double-list of atoms floor, empty,
%% @throws shadows.
%% and occupied.

parse(GridStr) ->
    Lines = string:lexemes(GridStr, "\n"),
    [parse_line(Line) || Line <- Lines].

parse_line(Line) ->
    [parse_symbol(Symbol) || Symbol <- Line].

parse_symbol($.) -> floor;
parse_symbol($L) -> empty;
parse_symbol($#) -> occupied.

%% @doc Lists sorted adjacent tiles' coordinates
adjacent({MaxX, MaxY}, {X, Y}) ->
    lists:sort([
     {AdjX, AdjY} ||
     AdjX <- [X - 1, X, X + 1],
     AdjY <- [Y - 1, Y, Y + 1],
     {AdjX, AdjY} =/= {X, Y},  % self is not adjacent
     AdjX > 0, AdjY > 0,
     AdjX =< MaxX, AdjY =< MaxY
    ]).

-spec at(Grid, {X, Y}) -> Tile when
      Grid :: grid(),
      X :: integer(),
      Y :: integer(),
      Tile :: tile().
%% @doc Shows what is at coordinates {X, Y}
at(Grid, {X, Y}) ->
    lists:nth(Y, lists:nth(X, Grid)).

%% @doc Produces the next generation tile at {X, Y}
next(Grid, {X, Y}) ->
    MaxX = length(Grid),
    MaxY = length(lists:nth(1, Grid)),
    Tile = at(Grid, {X, Y}),
    Adjacent = [at(Grid, {AdjX, AdjY})
                || {AdjX, AdjY} <- adjacent({MaxX, MaxY},
                                                 {X, Y})],
    next2(Tile, Adjacent).

next2(floor, _Adjacent) -> floor;
next2(empty, Adjacent) ->
    case lists:member(occupied, Adjacent) of
        false -> occupied;
        true -> empty
    end;
next2(occupied, Adjacent) ->
    case count(occupied, Adjacent) >= 4 of
        true -> empty;
        false -> occupied
    end.

count(Needle, Haystack) ->
    lists:foldl(
      fun(El, Acc) ->
              case El =:= Needle of 
                  true -> Count + 1 ;
                  false -> Count
              end
      end, 0, Haystack),

