-module(day12_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE_INSTRUCTIONS,
"F10
N3
F7
R90
F11").

parse_test_() ->
    [

     {"Parse example instructions.",
      ?_assertEqual([{f, 10}, {n, 3}, {f, 7}, {r, 90}, {f, 11}],
                    day12:parse(?EXAMPLE_INSTRUCTIONS))}

    ].
