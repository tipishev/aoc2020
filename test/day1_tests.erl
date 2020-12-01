-module(day1_tests).
-include_lib("eunit/include/eunit.hrl").

part1_test_() ->
    ?_assertEqual({299, 1721},
                  day1:sum2020([1721, 979, 366, 299, 675, 1456])).

part2_test_() ->
    ?_assertEqual({366, 675, 979},
                  day1:sum2020_p2([1721, 979, 366, 299, 675, 1456])).
