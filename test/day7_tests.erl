-module(day7_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE,
"light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.").

parse_test_() ->
    [

     {"Example test",
      ?_assertEqual(

         [
          {"light red", [{"bright white", 1}, {"muted yellow", 2}]},
          {"dark orange", [{"bright white", 3}, {"muted yellow", 4}]},
          {"bright white", [{"shiny gold", 1}]},
          {"muted yellow", [{"faded blue", 9}, {"shiny gold", 2}]},
          {"shiny gold", [{"dark olive", 1}, {"vibrant plum", 2}]},
          {"dark olive", [{"dotted black", 4}, {"faded blue", 3}]},
          {"vibrant plum", [{"dotted black", 6}, {"faded blue", 5}]},
          {"faded blue", []},
          {"dotted black", []}
         ], day7:parse(?EXAMPLE))}

    ].

part1_test_() ->
    [

     {"Example test",
      ?_assertEqual(4, day7:solve_part1(?EXAMPLE))}

    ].

part2_test_() ->
    [

     {"Example test",
      ?_assertEqual(32, day7:solve_part2(?EXAMPLE))}

    ].
