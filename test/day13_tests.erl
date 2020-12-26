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
          {a, 0, n, 7},
          {a, 12, n, 13},
          {a, 12, n, 19},
          {a, 25, n, 31},
          {a, 55, n, 59}
         ],
         day13:parse_congruences(?EXAMPLE))}

    ].

solve_congruences_test_() ->
    [

     {"Solve the toy system of congruences from Wikipedia.",
      ?_assertEqual(
         {a, 39, n, 60},
         day13:solve_congruences([
          {a, 0, n, 3}
          ,{a, 3, n, 4}
          ,{a, 4, n, 5}
         ]))}

     ,{"Solve the system of congruences from example.",
      ?_assertEqual(
         {a, 1068781, n, 3162341},
         day13:solve_congruences([
          {a, 0, n, 7},
          {a, 12, n, 13},
          {a, 12, n, 19},
          {a, 25, n, 31},
          {a, 55, n, 59}
         ]))}

    ].


extended_gcd_test_() ->
    [

     {"Wikipedia example of 240 and 46.",
      ?_assertEqual(
         {m1,-9,m2,47,gcd,2,quotA,-120,quotB,23},
         day13:extended_gcd(240, 46))}


    ].

earliest_test_() ->
    [

     {"The puzzle example.",
      ?_assertEqual({wait, 5, bus, 59},
                    day13:earliest(day13:parse(?EXAMPLE)))}
    ].

