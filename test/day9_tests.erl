-module(day9_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE, [35, 20, 15, 25, 47, 40, 62, 55, 65,
                  95, 102, 117, 150, 182, 127, 219,
                  299, 277, 309, 576]).

part1_test_() ->
    [

     {"Example test",
      ?_assertEqual(127, day9:check_xmas(?EXAMPLE, 5))}

    ].

part2_test_() ->
    [

     {"Example test",
      ?_assertEqual({15, 47}, day9:find_weakness(?EXAMPLE, 127))}

    ].
