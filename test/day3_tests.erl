-module(day3_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_MAP, [
"..##.......",
"#...#...#..",
".#....#..#.",
"..#.#...#.#",
".#...##..#.",
"..#.##.....",
".#.#.#....#",
".#........#",
"#.##...#...",
"#...##....#",
".#..#...#.#"]).

is_tree_test_() ->
    [

     {"Empty starting point test",
     ?_assertNot(day3:is_tree(?TEST_MAP, 1, 1))},

     {"The closest tree",
     ?_assert(day3:is_tree(?TEST_MAP, 2, 1))}
    ].
