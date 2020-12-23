%% @author Timofey Tipishev <tipishev@gmail.com>
%% @doc This is AOC2020 <em>day11</em> solution
%% @reference <a href="https://adventofcode.com/2020/day/11">AOC 2020 day 11</a> for
%% @since 2020-12-18
%% @version 0.5.0

-module(day11).

-export([solve_part1/1, solve_part2/1]).

% could be shielded with -ifdef(TEST) macro
-export([parse/1, adjacent/2, at/2, next/2, next/1,
         count_occupied/1, visible/2]).

%%% solution

solve_part1(Input) ->
    part1(parse(Input)).

solve_part2(_Input) ->
    undefined.

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
%% @doc Shows what is at coordinates {X, Y}.
at(Grid, {X, Y}) ->
    lists:nth(Y, lists:nth(X, Grid)).

-spec visible(Grid, {X, Y}) -> Tiles when
      Grid :: grid(),
      X :: integer(),
      Y :: integer(),
      Tiles :: [tile()].

%% @doc Lists non-floor tiles visible from {X, Y}.
visible(Grid, Origin) ->
    WithWalls = [arrow(Grid, Origin, Vector) ||
     Vector <- [{1, 0}, {1, 1}, {0, 1}, {-1, 1},
                {-1, 0}, {-1, -1}, {0, -1}, {1, -1}]
    ],
    WithoutWalls = [Tile || Tile <- WithWalls, Tile =/= wall],
    WithoutWalls.

%% @doc Shoots an arrow from {X, Y} in the direction of {Dx, Dy}
%% returns either `empty`, `occupied`, or `wall`.
arrow(Grid, _Origin={X, Y}, Vector={Dx, Dy}) ->
    {MaxX, MaxY} = dimensions(Grid),
    NewOrigin = {NewX, NewY} = {X + Dx, Y + Dy},
    case lists:member(NewX, [0, MaxX + 1])
          or lists:member(NewY, [0, MaxY + 1]) of
          true -> wall;
          false ->
            case at(Grid, {NewX, NewY}) of
                empty -> empty;
                occupied -> occupied;
                floor -> arrow(Grid, NewOrigin, Vector)
            end
    end.


%% @doc Measures X, Y dimensions of the Grid.
dimensions(Grid) ->
    {length(Grid), length(lists:nth(1, Grid))}.

%% @doc Advances grid to the next generation.
next(Grid) ->
    {MaxX, MaxY} = dimensions(Grid),
    [
     [next(Grid, {Row, Col})
      || Col <- lists:seq(1, MaxY)]
     || Row <- lists:seq(1, MaxX)
    ].


%% @doc Produces the next generation tile at {X, Y}.
next(Grid, {X, Y}) ->
    {MaxX, MaxY} = dimensions(Grid),
    Tile = at(Grid, {X, Y}),
    Adjacent = [at(Grid, {AdjX, AdjY})
                || {AdjX, AdjY} <- adjacent({MaxX, MaxY},
                                                 {X, Y})],
    next2(Tile, Adjacent).

%% @doc Produces the next generation tile given current and
%% adjacent tiles.
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

%% @doc Counts the number of occupied seats.
count_occupied(Grid) ->
    count(occupied, lists:flatten(Grid)).

count(_, []) -> 0;
count(X, [X | XS]) -> 1 + count(X, XS);
count(X, [_|XS]) -> count(X, XS).

%% @doc Counts occupied seats after equilibrium.
part1(Grid) ->
    NewGrid = next(Grid),
    case NewGrid =:= Grid of 
        true -> count_occupied(Grid);
        false -> part1(NewGrid)
    end.
