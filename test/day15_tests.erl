-module(day15_tests).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    [

     {"Example parsing test.",
      ?_assertEqual([0, 3, 6], day15:parse("0,3,6"))}

    ].

number_spoken_test_() ->
    [

     {"First Starting Number is spoken first.",
      ?_assertEqual(0, day15:spoken([0, 3, 6], 1))}

     ,{"Second Starting Number is spoken second.",
      ?_assertEqual(3, day15:spoken([0, 3, 6], 2))}

     ,{"If Most Recently Spoken Number (MRSN) is new, spoken 0.",
      ?_assertEqual(0, day15:spoken([0, 3, 6], 4))}

     ,{"If the MRSN is old, next is turns apart.",
      ?_assertEqual(3, day15:spoken([0, 3, 6], 5))}

     ,{"Turn 6",
      ?_assertEqual(3, day15:spoken([0, 3, 6], 6))}

     ,{"Turn 7",
      ?_assertEqual(1, day15:spoken([0, 3, 6], 7))}

     ,{"Turn 2020",
      ?_assertEqual(436, day15:spoken([0, 3, 6], 2020))}

    ].

enumerate_test_() ->
    [

     {"Example parsing test.",
      ?_assertEqual([{3, 1}, {14, 2}, {15, 3}],
                    day15:enumerate([3, 14, 15]))}

    ].
