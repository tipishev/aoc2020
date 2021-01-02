-module(day16_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE,
"class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12").

parse_test_() ->
    [

     {"Parse the example.",
      ?_assertEqual(
         {fields, #{class => [{1, 3}, {5, 7}],
                    row => [{6, 11}, {33, 44}],
                    seat => [{13, 40}, {45, 50}]},
          your, [7, 1, 14],
          nearby, [[7, 3, 47],
                   [40, 4, 50],
                   [55, 2, 20],
                   [38, 6, 12] ]
         },
         day16:parse(?EXAMPLE))}
    ].

fields_to_lists_test_() ->
    [

     {"Convert fields to a sorted list of allowed values.",
      ?_assertEqual(
         [1, 2, 3, 42, 43, 44, 45],
         day16:to_valid_values(#{foo => {1, 3},
                                 bar => {42, 45}}))}
    ].

part1_test_() ->
    [

     {"Check example input",
      ?_assertEqual( 71,
                     day16:solve_part1(?EXAMPLE))}
    ].

filter_test_() ->
    [

     {"Filter tickets",
      ?_assertEqual( [[1, 2, 3], [5, 2, 1]],
                     day16:filter(
                       _Tickets=[[1, 2, 3], [5, 2, 1], [1, 3, 4]],
                       _ValidValues=[1, 2, 3, 5]))}
    ].

transpose_test_() ->
    [

     {"Test matrix transposition.",
      ?_assertEqual( [[1, 2, 3], [4, 5, 6]],
                     day16:transpose([[1, 4], [2, 5], [3, 6]]))
     }].

% deduce_fields_test_() ->
%     [

%      {"Deduce fields for example input",
%       ?_assertEqual([row, class, seat],
%                      day16:deduce(day16:parse(?EXAMPLE)))}
%     ].
