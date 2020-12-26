-module(day13_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE,
"939
7,13,x,x,59,x,31,19").

parse_test_() ->
    [

     {"Parse the example input.",
      ?_assertEqual({earliest, 939,
                     buses, [7, 13, 59, 31, 19]},
                    day13:parse(?EXAMPLE))}

    ].

parse2_test_() ->
    [

     {"Parse the example as a sorted system of congruences.",
      ?_assertEqual(
         [
          {n, 7, a, 0},
          {n, 13, a, 12},
          {n, 19, a, 12},
          {n, 31, a, 25},
          {n, 59, a, 55}
         ],
         day13:parse_congruences(?EXAMPLE))}

    ].

earliest_test_() ->
    [

     {"The puzzle example.",
      ?_assertEqual({wait, 5, bus, 59},
                    day13:earliest(day13:parse(?EXAMPLE)))}
    ].

