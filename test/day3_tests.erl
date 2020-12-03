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

     {"Out of bounds",
     ?_assertEqual(out_of_bounds, day3:is_tree(?TEST_MAP, 12, 1))},

     {"Empty starting point test",
     ?_assertNot(day3:is_tree(?TEST_MAP, 1, 1))},

     {"The closest tree",
     ?_assert(day3:is_tree(?TEST_MAP, 2, 1))},

     {"Same-line wrapping negative",
     ?_assertNot(day3:is_tree(?TEST_MAP, 1, 12))},

     {"Same-line wrapping positive",
     ?_assert(day3:is_tree(?TEST_MAP, 1, 14))}

    ].

tree_count_test_() ->
    [

     {"Example",
     ?_assertEqual(7, day3:count_trees(?TEST_MAP, 1, 3))}

    ].

