-module(day14_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE, "").

parse_test_() ->
    [

     {"Example test",
      ?_assertEqual(undefined, day14:solve_part1(?EXAMPLE))}

    ].
