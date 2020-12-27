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
         #{7 => 101, 8 => 64},
         day14:dock(day14:parse(?EXAMPLE))
        )
     }

     ,{"Test example docking program's checksum.",
      ?_assertEqual(
         165,
         day14:map_sum(day14:dock(day14:parse(?EXAMPLE)))
        )
     }

    ].

dock2_test_() ->
    [

     {"Test example docking program version 2.",
      ?_assertEqual(
         #{16 => 1,17 => 1,18 => 1,19 => 1,24 => 1,25 => 1,26 => 1,
           27 => 1,58 => 100, 59 => 100}
         ,
         day14:dock2(day14:parse(?EXAMPLE2))
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

mask_address_test_() ->
    [

     {"Test masking address with the example mask.",
      ?_assertEqual(
         "000000000000000000000000000000X1101X",
         day14:mask_address("000000000000000000000000000000101010",
                            "000000000000000000000000000000X1001X")
        )
     }

    ].


expand_address_test_() ->
    [

     {"Small expansion test.",
      ?_assertEqual(
         [ "0010" ,"0011" ,"0110" ,"0111" ],
         day14:expand("0X1X")
        )}

      ,{"Test masking 11 with example mask",
        ?_assertEqual(
         [
          "000000000000000000000000000000011010"
          ,"000000000000000000000000000000011011"
          ,"000000000000000000000000000000111010"
          ,"000000000000000000000000000000111011"
         ],
         day14:expand("000000000000000000000000000000X1101X")
        )}

    ].
