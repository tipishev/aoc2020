-module(day9).

-export([solve_part1/1, solve_part2/1]).
-export([check_xmas/2]).

%%% solution

solve_part1(Input) ->
    check_xmas(Input, 25).

solve_part2(_Input) ->
    undefined.

check_xmas(Numbers, PreambleLength) ->
    Preamble = lists:sublist(Numbers, PreambleLength),
    Tail = lists:nthtail(PreambleLength, Numbers),
    {invalid, Result} = check_xmas2(Preamble, Tail),
    Result.

check_xmas2(_, []) -> ok;  % rather unexpected though
check_xmas2(Window, [Head|Tail]) ->
    ValidSums = sets:from_list([A + B || A <- Window, B <- Window]),
    case sets:is_element(Head, ValidSums) of
        true ->
            NewWindow = lists:nthtail(1, Window) ++ [Head],
            check_xmas2(NewWindow, Tail);
        false ->
            {invalid, Head}
    end.

