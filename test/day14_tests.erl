-module(day14_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE,
"mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0").

parse_test_() ->
    [

     {"Parse example program.",
      ?_assertEqual(
         [
          {mask, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"},
          {mem, 8, 11},
          {mem, 7, 101},
          {mem, 8, 0}
         ],
         day14:parse(?EXAMPLE))}

    ].
