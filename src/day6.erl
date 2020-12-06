-module(day6).

-export([solve_part1/1, solve_part2/1]).

% for tests
-export([]).

%%% solution

solve_part1(Input) ->
    Parsed = parse(Input),
    Deduplicated = lists:map(fun deduplicate/1, Parsed),
    lists:sum(lists:map(fun length/1, Deduplicated)).

solve_part2(Input) ->
    Groups = parse2(Input),
    Intersections = lists:map(fun group_intersection/1, Groups),
    lists:sum(lists:map(fun length/1, Intersections)).

%%% internals
parse(Input) ->
    Csv = re:replace(Input, "\n\n", ",", [global, {return, list}]),
    WithoutNewlines = re:replace(Csv, "\n", " ", [global, {return, list}]),
    WithSpaces = string:lexemes(WithoutNewlines, ","),
    WithoutSpaces = lists:map(fun remove_spaces/1, WithSpaces),
    WithoutSpaces.

parse2(Input) ->
    Csv = re:replace(Input, "\n\n", ",", [global, {return, list}]),
    GroupStrings = string:lexemes(Csv, ","),
    Groups = lists:map(fun(Str) -> string:lexemes(Str, "\n") end,
                       GroupStrings),
    Groups.

group_intersection(Group) ->
    Sets = lists:map(fun sets:from_list/1, Group),
    Intersection = sets:intersection(Sets),
    sets:to_list(Intersection).

deduplicate(StringWithRepetitions) ->
    Sets = sets:from_list(StringWithRepetitions),
    sets:to_list(Sets).

remove_spaces(String) ->
    re:replace(String, " ", "", [global, {return, list}]).
