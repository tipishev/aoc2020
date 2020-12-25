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
      ?_assertEqual([{forward, 10}, {north, 3}, {forward, 7},
                     {right, 90}, {forward, 11}],
                    day12:parse(?EXAMPLE_INSTRUCTIONS))}

    ].

cruise_test_() ->
    [

     {"Cruise using example instructions, should arrive at {17, -8}, heading South",
      ?_assertEqual({{17, -8}, 270},
                    day12:cruise({0, 0}, 0, day12:parse(?EXAMPLE_INSTRUCTIONS)))}

    ].

cruise_wp_test_() ->
    [

     {"Waypoint-cruise using example instructions, arrive at {17, -8}, heading S",
      ?_assertEqual({{214, -72}, {4, -10}},
                    day12:wp_cruise({0, 0}, {1, 10},
                                    day12:parse(?EXAMPLE_INSTRUCTIONS)))}

    ].

turn_test_() ->
    [

     {"Turn 0 from East should head East",
      ?_assertEqual(0, day12:turn(0, 0))},

     {"Turn +90 from East should head North",
      ?_assertEqual(90, day12:turn(0, 90))},

     {"Turn 270 from East should head South",
      ?_assertEqual(270, day12:turn(0, 270))},

     {"Turn 180 from East should head West",
      ?_assertEqual(180, day12:turn(0, 180))},

     {"Turn 360 from East should head East",
      ?_assertEqual(0, day12:turn(0, 360))},

     {"Turn 540 from East should head West",
      ?_assertEqual(180, day12:turn(0, 540))}
    ].

rotate_test_() ->
    [

     {
      "Turn {10, 1} 90 counter-clockwise should give {-1, 10}",
      ?_assertEqual({-1, 10}, day12:rotate({10, 1}, 90))
     },

     {
      "Turn {10, 1} 90 clockwise should give {-1, 10}",
      ?_assertEqual({1, -10}, day12:rotate({10, 1}, -90))
     },

     {
      "Turn {10, 1} 360 degrees should give {10, 1}",
      ?_assertEqual({10, 1}, day12:rotate({10, 1}, 360))
     }

    ].
