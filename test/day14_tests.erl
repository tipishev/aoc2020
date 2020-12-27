-module(day14_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE,
"mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0").

-define(EXAMPLE2,
"mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1").

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

dock_test_() ->
    [

     {"Test example docking program.",
      ?_assertEqual(
         #{7 => "000000000000000000000000000001100101",
           8 => "000000000000000000000000000001000000"},
         day14:dock(day14:parse(?EXAMPLE))
        )
     },

     {"Test example docking program's checksum.",
      ?_assertEqual(
         165,
         day14:map_sum(day14:dock(day14:parse(?EXAMPLE)))
        )
     }

    ].

bin36_test_() ->
    [

     {"Test converting 11 to a padded binary.",
      ?_assertEqual(
         "000000000000000000000000000000001011",
         day14:bin36(11)
        )
     }

    ].

mask_value_test_() ->
    [

     {"Test masking 11 with the example mask.",
      ?_assertEqual(
         "000000000000000000000000000001001001",
         day14:mask_value("000000000000000000000000000000001011",
                          "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
        )
     }

    ].
