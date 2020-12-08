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

-define(EXAMPLE_INSTRUCTIONS,
	[{nop,0},
	 {acc,1},
	 {jmp,4},
	 {acc,3},
	 {jmp,-3},
	 {acc,-99},
	 {acc,1},
	 {jmp,-4},
	 {acc,6}]).

parse_test_() ->
    [

     {"Parse example input",
      ?_assertEqual(?EXAMPLE_INSTRUCTIONS, day8:parse(?EXAMPLE))}

    ].


part1_test_() ->
    [

     {"Part 1 example",
      ?_assertEqual({loops, 5}, day8:detect_loop(?EXAMPLE_INSTRUCTIONS))}

    ].

part2_test_() ->
    [

     {"Part 2 example",
      ?_assertEqual(8, day8:fix(?EXAMPLE_INSTRUCTIONS))}

    ].
