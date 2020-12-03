-module(day3).

%% aoc_solution callbacks
-export([solve_part1/1, solve_part2/1]).

%% for tests
-export([is_tree/3]).

%%% solution behavior

solve_part1(_Input) -> undefined.


solve_part2(_Input) -> undefined.

%%% internals

-spec is_tree(Map, X, Y) -> IsTree when
      Map :: list(list(pos_integer())),
      X :: integer(),
      Y :: integer(),
      IsTree :: boolean() | out_of_bounds.

is_tree(Map, X, _) when X > length(Map)-> out_of_bounds;
is_tree(Map, X, Y) ->
    Line = lists:nth(X, Map),
    Symbol = lists:nth(Y, Line),
    Symbol =:= $#.
