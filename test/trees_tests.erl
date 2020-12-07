-module(trees_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE, "").

creation_test_() ->
    [

     {"Create a simple brown bag with quantity 12",
      ?_assertEqual({tree,"brown",12,[]}, trees:new("brown", 12))}

    ].
