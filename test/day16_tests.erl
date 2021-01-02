-module(day16_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE,
"class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12").

parse_test_() ->
    [

     {"Parse",
      ?_assertEqual(
         {fields, #{class => [{1, 3}, {5, 7}],
                    row => [{6, 11}, {33, 44}],
                    seat => [{13, 40}, {45, 50}]},
          your, [7, 1, 14],
          nearby, [[7, 13, 47],
                   [40, 4, 50],
                   [55, 2, 20],
                   [38, 6, 12] ]
         },
         day16:parse(?EXAMPLE))}

    ].
