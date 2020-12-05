-module(day5).

-export([solve_part1/1, solve_part2/1]).

% for tests
-export([to_row_col/1, half/2]).

%%% solution

solve_part1(Codes) -> 
    RowCols = lists:map(fun to_row_col/1, Codes),
    SeatIds = lists:map(fun seat_id/1, RowCols),
    lists:max(SeatIds).

solve_part2(Codes) ->
    RowCols = lists:map(fun to_row_col/1, Codes),
    SeatIds = lists:map(fun seat_id/1, RowCols),
    SortedSeatIds = lists:sort(SeatIds),
    detect_anomaly(SortedSeatIds).

%%% internals

detect_anomaly([Head | [Neck | Tail]]) when Neck =:= Head + 1 ->
    detect_anomaly([Neck | Tail]);  % let's keep looking
detect_anomaly([Head | [Anomaly | _]]) -> (Head + Anomaly) div 2.

half(lower, {Low, High}) ->
    {Low, Low + (High - Low + 1) div 2 - 1};
half(upper, {Low, High}) ->
    {Low + (High - Low + 1) div 2, High}.

to_row_col(BinCode) ->
    to_row_col(BinCode, {0, 127}, {0, 7}).

to_row_col([], {Row, Row}, {Col, Col}) ->
    {Row, Col};
to_row_col([$F|Tail], RowRange, ColRange) ->
    to_row_col(Tail, half(lower, RowRange), ColRange);
to_row_col([$B|Tail], RowRange, ColRange) ->
    to_row_col(Tail, half(upper, RowRange), ColRange);
to_row_col([$L|Tail], RowRange, ColRange) ->
    to_row_col(Tail, RowRange, half(lower, ColRange));
to_row_col([$R|Tail], RowRange, ColRange) ->
    to_row_col(Tail, RowRange, half(upper, ColRange)).

seat_id({Row, Col}) -> Row * 8 + Col.
