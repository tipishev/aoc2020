-module(trees_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE, "").

part1_test_() ->
    [

     {"Example test",
      ?_assertEqual(undefined, day9:solve_part1(?EXAMPLE))}

    ].
