-module(day2_tests).
-include_lib("eunit/include/eunit.hrl").

count_test_() ->
    [{
     "Zimple abazaba tezt",
     ?_assertEqual(4, day2:count("a", "abazaba"))}
    ].

part1_test_() ->
    [
     ?_assert(day2:validate("a", 1, 3, "abcde")),
     ?_assertNot(day2:validate("b", 1, 3, "cdefg")),
     ?_assert(day2:validate("c", 2, 9, "ccccccccc"))
    ].

is_char_at_test_() ->
    [
     {"Simple positive",
     ?_assert(day2:is_char_at("a", 2, "cat"))},

     {"Simple negative",
     ?_assertNot(day2:is_char_at("a", 2, "dog"))}

    ].

part2_test_() ->
    [
     ?_assert(day2:validate2("a", 1, 3, "abcde")),
     ?_assertNot(day2:validate2("b", 1, 3, "cdefg")),
     ?_assertNot(day2:validate2("c", 2, 9, "ccccccccc"))
    ].
