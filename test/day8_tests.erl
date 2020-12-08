-module(day8_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE,
"nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6").

parse_test_() ->
    [

     {"Example test",
      ?_assertEqual(
         [{nop,0},
          {acc,1},
          {jmp,4},
          {acc,3},
          {jmp,-3},
          {acc,-99},
          {acc,1},
          {jmp,-4},
          {acc,6}], day8:parse(?EXAMPLE))}

    ].
