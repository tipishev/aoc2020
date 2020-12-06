-module(day6_tests).
-include_lib("eunit/include/eunit.hrl").

part1_test_() ->
    [

     {"BFFFBBFRRR: row 70, column 7",
      ?_assertEqual({70, 7}, day5:to_row_col("BFFFBBFRRR"))},

     {"FFFBBBFRRR: row 14, column 7",
      ?_assertEqual({14, 7}, day5:to_row_col("FFFBBBFRRR"))},

     {"BBFFBBFRLL: row 102, column 4",
      ?_assertEqual({102, 4}, day5:to_row_col("BBFFBBFRLL"))}
    ].
