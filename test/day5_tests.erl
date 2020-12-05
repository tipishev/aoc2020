-module(day5_tests).
-include_lib("eunit/include/eunit.hrl").

range_halve_test_() ->
    [

     {"Pick lower range (small)",
      ?_assertEqual({0, 1}, day5:half(lower, {0, 3}))},

     {"Pick upper range (big)",
      ?_assertEqual({2, 3}, day5:half(upper, {0, 3}))},

     {"Pick lower range (big)",
      ?_assertEqual({0, 63}, day5:half(lower, {0, 127}))},

     {"Pick upper range (big)",
      ?_assertEqual({64, 127}, day5:half(upper, {0, 127}))},

     {"Lower, mid-range",
      ?_assertEqual({32, 47}, day5:half(lower, {32, 63}))},

     {"Upper, mid-range",
      ?_assertEqual({44, 47}, day5:half(upper, {40, 47}))}

    ].

binary_to_row_col_test_() ->
    [

     {"BFFFBBFRRR: row 70, column 7",
      ?_assertEqual({70, 7}, day5:to_row_col("BFFFBBFRRR"))},

     {"FFFBBBFRRR: row 14, column 7",
      ?_assertEqual({14, 7}, day5:to_row_col("FFFBBBFRRR"))},

     {"BBFFBBFRLL: row 102, column 4",
      ?_assertEqual({102, 4}, day5:to_row_col("BBFFBBFRLL"))}
    ].
