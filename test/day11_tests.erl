-module(day11_tests).
-include_lib("eunit/include/eunit.hrl").

%%% part1 fixtures

-define(GEN0,
"L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL").

-define(GEN1,
"#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##").

-define(GEN2,
"#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##").


-define(GEN3,
"#.##.L#.##
#L###LL.L#
L.#.#..#..
#L##.##.L#
#.##.LL.LL
#.###L#.##
..#.#.....
#L######L#
#.LL###L.L
#.#L###.##").

-define(GEN4,
"#.#L.L#.##
#LLL#LL.L#
L.L.L..#..
#LLL.##.L#
#.LL.LL.LL
#.LL#L#.##
..L.L.....
#L#LLLL#L#
#.LLLLLL.L
#.#L#L#.##").

-define(GEN5,
"#.#L.L#.##
#LLL#LL.L#
L.#.L..#..
#L##.##.L#
#.#L.LL.LL
#.#L#L#.##
..L.L.....
#L#L##L#L#
#.LLLLLL.L
#.#L#L#.##").

%%% part 2 fixtures

-define(VISIBILITY_EXAMPLE1,
".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#.....").

-define(VISIBILITY_EXAMPLE2,
".............
.L.L.#.#.#.#.
.............").

-define(VISIBILITY_EXAMPLE3,
".##.##.
#.#.#.#
##...##
...L...
##...##
#.#.#.#
.##.##.").


-define(VIS0,
"L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL").

-define(VIS1,
"#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##").

-define(VIS2,
"#.##.##.##
#.LL.LL.L#
#LLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLLL.L
#.LLLLL.L#").

-define(VIS3,
"#.L#.##.L#
#L#####.LL
L.#.#..#..
##L#.##.##
#.##.#L.##
#.#####.#L
..#.#.....
LLL####LL#
#.L#####.L
#.L####.L#").

-define(VIS4,
"#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##LL.LL.L#
L.LL.LL.L#
#.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLL#.L
#.L#LL#.L#").

-define(VIS5,
"#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##L#.#L.L#
L.L#.#L.L#
#.L####.LL
..#.#.....
LLL###LLL#
#.LLLLL#.L
#.L#LL#.L#").

-define(VIS6,
"#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##L#.#L.L#
L.L#.LL.L#
#.LLLL#.LL
..#.L.....
LLL###LLL#
#.LLLLL#.L
#.L#LL#.L#").


%%% Tests


adjacent_test_() ->
    [

     {"Corner test

      *..
      ...
      ...

      ",
      ?_assertEqual([
                     {1, 2},
                     {2, 1}, {2, 2}
                    ],
                    day11:adjacent({3, 3}, {1, 1}))},

     {"Corner test

      .*.
      ...
      ...

      ",
      ?_assertEqual([
                     {1, 1}, {1, 3},
                     {2, 1}, {2, 2}, {2, 3}
                    ],
                    day11:adjacent({3, 3}, {1, 2}))},

     {"Center test

      ...
      .*.
      ...

      ",
      ?_assertEqual([
                     {1, 1}, {1, 2}, {1, 3},
                     {2, 1}, {2, 3},
                     {3, 1}, {3, 2}, {3, 3}
                    ],
                    day11:adjacent({3, 3}, {2, 2}))}

    ].

parse_test_() ->
    [

     {"Parse a simple board string",
      ?_assertEqual(
         [
          [empty, floor],
          [floor, occupied]
         ],
         day11:parse(
"L.
.#"))}

    ].

at_test_() ->
    [

     {"Check that bottom-left corner is floor",
      ?_assertEqual(floor,
         day11:at([
          [empty, empty],
          [floor, occupied]
         ], {2, 1}))}

    ].

next_adj_test_() ->
    [

     {"Check seat becoming occupied",
      ?_assertEqual(occupied,
         day11:next_adj(day11:parse(?GEN0), {1, 1}))},

     {"Check floor not changing",
      ?_assertEqual(floor,
         day11:next_adj(day11:parse(?GEN0), {1, 2}))},

     {"Check freeing-up",
      ?_assertEqual(empty,
         day11:next_adj(day11:parse(?GEN1), {1, 3}))}

    ].

next_grid_test_() ->
    [

     {"GEN0 -> GEN1",
      ?_assertEqual(day11:parse(?GEN1),
                    day11:next(day11:parse(?GEN0),
                               fun day11:next_adj/2))},

     {"GEN1 -> GEN2",
      ?_assertEqual(day11:parse(?GEN2),
                    day11:next(day11:parse(?GEN1),
                               fun day11:next_adj/2))},

     {"GEN2 -> GEN3",
      ?_assertEqual(day11:parse(?GEN3),
                    day11:next(day11:parse(?GEN2),
                               fun day11:next_adj/2))}

    ].

count_occupied_test_() ->
    [

     {"Simple test",
     ?_assertEqual(2, day11:count_occupied([[empty, occupied],
                                            [occupied, floor]]))},

     {"Example test",
     ?_assertEqual(37, day11:count_occupied(day11:parse(?GEN5)))}

    ].

visibility_test_() ->
    [

     {
      "empty seat below would see eight occupied seats",
      ?_assertEqual(lists:duplicate(8, occupied),
                    day11:visible(day11:parse(?VISIBILITY_EXAMPLE1), {5, 4}))
     },

     {
      "The leftmost empty seat below would only see one empty seat,"
      " but cannot see any of the occupied ones.",
      ?_assertEqual([empty],
                    day11:visible(day11:parse(?VISIBILITY_EXAMPLE2), {2, 2}))
     },

     {
      "The empty seat below would see no occupied seats.",
      ?_assertEqual([],
                    day11:visible(day11:parse(?VISIBILITY_EXAMPLE3), {4, 4}))
     }

    ].
