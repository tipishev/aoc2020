-module(day10_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SMALL_EXAMPLE, [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]).

-define(LARGER_EXAMPLE,
        [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19,
         38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]).

%%% Part 1

part1_test_() ->
    [

     {"Small example",
      ?_assertEqual([{1, 7}, {3, 5}],
                    day10:jolt_diff_distro(?SMALL_EXAMPLE))},

     {"Larger example",
      ?_assertEqual([{1, 22}, {3, 10}],
                    day10:jolt_diff_distro(?LARGER_EXAMPLE))}

    ].

%%% Part 2

part2_test_() ->
    [

     {"Small example",
      ?_assertEqual(8,
                    day10:solve_part2(?SMALL_EXAMPLE))}

     ,{"Larger example",
      ?_assertEqual(19208,
                    day10:solve_part2(?LARGER_EXAMPLE))}

    ].
