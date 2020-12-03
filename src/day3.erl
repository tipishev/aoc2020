-module(day3).

-export([solve_part1/1, solve_part2/1]).

%% for tests
-export([is_tree/3, count_trees/3]).

%%% solution behavior

solve_part1(Map) -> count_trees(Map, 1, 3).


solve_part2(Map) ->
    ToCheck = [{1, 1}, {1, 3}, {1, 5},
               {1, 7}, {2, 1}],
    Counts = [count_trees(Map, Dx, Dy) || {Dx, Dy} <- ToCheck],
    lists:foldl(fun(El, Acc) -> El * Acc end, 1, Counts).

%%% internals

% restrict to empty spot (.) and tree (#)
-type slope_map() :: list(list(pos_integer())).

-spec count_trees(Map, Dx, Dy) ->
    TreeCount when
      Map :: slope_map(),
      Dx :: non_neg_integer(),
      Dy :: non_neg_integer(),
      TreeCount :: non_neg_integer().

count_trees(Map, Dx, Dy) ->
    count_trees(Map, Dx, Dy, 1, 1, 0).

-spec count_trees(Map, Dx, Dy, X, Y, CurrentCount) ->
    TreeCount when
      Map :: slope_map(),
      Dx :: non_neg_integer(),
      Dy :: non_neg_integer(),
      X :: non_neg_integer(),
      Y :: non_neg_integer(),
      CurrentCount :: non_neg_integer(),
      TreeCount :: non_neg_integer().

count_trees(Map, _Dx, _Dy, X, _Y, CurrentCount) when X > length(Map) ->
    CurrentCount;
count_trees(Map, Dx, Dy, X, Y, CurrentCount) ->
    case is_tree(Map, X, Y) of
        true -> count_trees(Map, Dx, Dy, X + Dx, Y + Dy, CurrentCount + 1);
        false -> count_trees(Map, Dx, Dy, X + Dx, Y + Dy, CurrentCount)
    end.


-spec is_tree(Map, X, Y) -> IsTree when
      Map :: slope_map(),
      X :: integer(),
      Y :: integer(),
      IsTree :: boolean() | out_of_bounds.

is_tree(Map, X, _) when X > length(Map)-> out_of_bounds;
is_tree(Map, X, Y) ->
    Line = lists:nth(X, Map),
    Ymod = case Y rem length(Line) of
      0 -> length(Line);  % no zero-indexing
      Other -> Other
    end,
    Symbol = lists:nth(Ymod, Line),
    Symbol =:= $#.
