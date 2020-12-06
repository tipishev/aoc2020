-module(day6_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE,
"abc

a
b
c

ab
ac

a
a
a
a

b").

part1_test_() ->
    [

     {"Example test",
      ?_assertEqual(11, day6:solve_part1(?EXAMPLE))}

    ].

part2_test_() ->
    [

     {"Example test",
      ?_assertEqual(6, day6:solve_part2(?EXAMPLE))}

    ].
