-module(day13_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE,
"939
7,13,x,x,59,x,31,19").

part1_test_() ->
    [

     {"The puzzle example.",
      ?_assertEqual(undefined, day13:solve_part1(?EXAMPLE))}

    ].

parse_test_() ->
    [

     {"Parse the example input.",
      ?_assertEqual({earliest, 939,
                     buses, [7, 13, 59, 31, 19]},
                    day13:parse(?EXAMPLE))}

    ].
