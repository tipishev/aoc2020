-module(day6).

-export([solve_part1/1, solve_part2/1]).

%%% solution

solve_part1(Input) ->
    Deduplicated = lists:map(fun deduplicate/1,  parse(Input)),
    sum_lengths(Deduplicated).

solve_part2(Input) ->
    Intersections = lists:map(fun group_intersection/1, parse2(Input)),
    sum_lengths(Intersections).

%%% internals

parse(Input) ->
    Csv = empty_lines_to_commas(Input),
    WithoutNewlines = re:replace(Csv, "\n", " ", [global, {return, list}]),
    WithSpaces = string:lexemes(WithoutNewlines, ","),
    _WithoutSpaces = lists:map(fun drop_spaces/1, WithSpaces).

parse2(Input) ->
    Csv = empty_lines_to_commas(Input),
    GroupStrings = string:lexemes(Csv, ","),
    _Groups = lists:map(fun(Str) -> string:lexemes(Str, "\n") end,
                        GroupStrings).

%% finds a list of elements shared by all group members
group_intersection(Group) ->
    Sets = lists:map(fun sets:from_list/1, Group),
    sets:to_list(sets:intersection(Sets)).

deduplicate(List) ->
    sets:to_list(sets:from_list(List)).

drop_spaces(String) ->
    re:replace(String, " ", "", [global, {return, list}]).

empty_lines_to_commas(String) ->
    re:replace(String, "\n\n", ",", [global, {return, list}]).

sum_lengths(List) ->
    lists:sum(lists:map(fun length/1, List)).
