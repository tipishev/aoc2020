-module(day11_tests).
-include_lib("eunit/include/eunit.hrl").

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


% part1_test_() ->
%     [

%      {"Example test",
%       ?_assertEqual(?GEN1, day11:life(?GEN0))}

%     ].

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
          [seat, floor],
          [floor, occupied]
         ],
         day11:parse(
"L.
.#"))}

    ].
