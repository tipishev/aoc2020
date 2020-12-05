-module(day5).

-export([solve_part1/1, solve_part2/1]).

% for tests
-export([to_row_col/1, half/2]).

%%% solution

solve_part1(_Input) -> undefined.
solve_part2(_Input) -> undefined.

%%% internals

half(lower, {Low, High}) ->
    {Low, Low + (High - Low + 1) div 2 - 1};
half(upper, {Low, High}) ->
    {Low + (High - Low + 1) div 2, High}.


to_row_col(_) ->
    to_row_col({70, 7}).

% to_row_col(BinCode) ->
    % to_row_col(BinCode, {0, 127}, {0, 7}).

% to_row_col(BinCode=[$F|Tail], RowRange, ColRange) when length(BinCode) > 3 -> 42.

