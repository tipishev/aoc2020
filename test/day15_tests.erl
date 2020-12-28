-module(day15_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE, "0,3,6").

parse_test_() ->
    [

     {"Example parsing test.",
      ?_assertEqual([0, 3, 6], day15:parse(?EXAMPLE))}

    ].

init_memory_test_() ->
    [

     {"Test initializing memory with starting numbers.",
      ?_assertEqual(#{1 => {once, 1}
                      ,2 => {once, 2}
                      ,3 => {once, 3}
                     }, day15:init_memory([1, 2, 3]))}


    ].
