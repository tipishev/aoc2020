-module(day9).

-export([solve_part1/1, solve_part2/1]).
-export([check_xmas/2, find_weakness/2]).

%%% solution

solve_part1(Input) ->
    check_xmas(Input, 25).

solve_part2(Input) ->
    {Min, Max} = find_weakness(Input, check_xmas(Input, 25)),
    Min + Max.

check_xmas(Numbers, PreambleLength) ->
    Preamble = lists:sublist(Numbers, PreambleLength),
    Tail = lists:nthtail(PreambleLength, Numbers),
    check_xmas2(Preamble, Tail).

check_xmas2(_, []) -> ok;  % rather unexpected though
check_xmas2(Window, [Head|Tail]) ->
    ValidSums = sets:from_list([A + B || A <- Window, B <- Window]),
    case sets:is_element(Head, ValidSums) of
        true ->
            NewWindow = lists:nthtail(1, Window) ++ [Head],
            check_xmas2(NewWindow, Tail);
        false -> Head
    end.

find_weakness(Numbers, NeededSum) ->
    find_weakness(Numbers, _Start=1, _Current=2,
                  _RunningSum=1, NeededSum).

find_weakness(Numbers, Start, _Current, _RunningSum, _NeededSum)
  when Start > length(Numbers) -> no_weakness;
find_weakness(Numbers, Start, Current, _RunningSum, NeededSum)
  when Current > length(Numbers) ->
    NewStart = Start + 1,
    NewRunningSum = lists:nth(NewStart, Numbers),
    find_weakness(Numbers, NewStart, NewStart + 1, NewRunningSum,
                  NeededSum);
find_weakness(Numbers, Start, Current, RunningSum, NeededSum) ->
    CurrentNumber = lists:nth(Current, Numbers),
    NewRunningSum = RunningSum + CurrentNumber,
    case NewRunningSum =:= NeededSum of
        true ->
            TargetRange = lists:sublist(Numbers, Start,
                                        Current - Start),
            {lists:min(TargetRange), lists:max(TargetRange)};
        false ->
            find_weakness(Numbers, Start, Current + 1,
                          NewRunningSum, NeededSum)
    end.


