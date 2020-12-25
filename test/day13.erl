-module(day13).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE,
"939
7,13,x,x,59,x,31,19").

part1_test_() ->
    [

     {"Example test",
      ?_assertEqual(undefined, day13:solve_part1(?EXAMPLE))}

    ].
