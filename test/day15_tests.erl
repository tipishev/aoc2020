-module(day15_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE, "0,3,6").

parse_test_() ->
    [

     {"Example parsing test.",
      ?_assertEqual([0, 3, 6], day15:parse(?EXAMPLE))}

    ].
