-module(day3).

-export([solve_part1/1, solve_part2/1]).
-export([is_tree/3, count_trees/3]).

%%% solution

solve_part1(Map) -> count_trees(Map, 1, 3).

solve_part2(Map) ->
    ToCheck = [{1, 1}, {1, 3}, {1, 5}, {1, 7}, {2, 1}],
    Counts = [count_trees(Map, Dx, Dy) || {Dx, Dy} <- ToCheck],
    lists:foldl(fun(El, Acc) -> El * Acc end, 1, Counts).

%%% internals

count_trees(Map, Dx, Dy) ->
    count_trees(Map, Dx, Dy, 1, 1, 0).

count_trees(Map, _Dx, _Dy, X, _Y, Count) when X > length(Map) ->
    Count;
count_trees(Map, Dx, Dy, X, Y, Count) ->
    NewCount = case is_tree(Map, X, Y) of
        true -> Count + 1;
        false -> Count
    end,
    count_trees(Map, Dx, Dy, X + Dx, Y + Dy, NewCount).

is_tree(Map, X, Y) ->
    Line = lists:nth(X, Map),
    Ymod = case Y rem length(Line) of
      0 -> length(Line);  % no zero-indexing
      Other -> Other
    end,
    Symbol = lists:nth(Ymod, Line),
    Symbol =:= $#.
