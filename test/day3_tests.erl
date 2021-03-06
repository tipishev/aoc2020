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
".#..#...#.#"]). % 11 x 11 grid

is_tree_test_() ->
    [

     {"Empty starting point test",
     ?_assertNot(day3:is_tree(?TEST_MAP, 1, 1))},

     {"The closest tree",
     ?_assert(day3:is_tree(?TEST_MAP, 2, 1))},

     {"Same-line wrapping negative",
     ?_assertNot(day3:is_tree(?TEST_MAP, 1, 12))},

     {"Same-line wrapping positive",
     ?_assert(day3:is_tree(?TEST_MAP, 1, 14))}

    ].

part1_test_() ->
    [

     {"Example",
     ?_assertEqual(7, day3:count_trees(?TEST_MAP, 1, 3))}
    ].

part2_test_() ->
    [

     {"In the above example, these slopes would find 2, 7, 3, 4,
      and 2 tree(s) respectively; multiplied together,
      these produce the answer 336.",
     ?_assertEqual(336, day3:solve_part2(?TEST_MAP))}
    ].
