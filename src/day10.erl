-module(day10).

-export([solve_part1/1, solve_part2/1]).

-export([jolt_diff_distro/1]).

%%% solution

solve_part1(_Input) ->
    undefined.

solve_part2(_Input) ->
    undefined.

jolt_diff_distro(BagAdapters) ->
    Outlet = 0,
    Joltages = lists:sort([Outlet | BagAdapters]),
    Diffs = diffs(Joltages, _Diffs=[]),
    count(Diffs).

diffs([_DeviceAdapter], Diffs) -> [3|Diffs];
diffs([Small, Big | Tail], Diffs) ->
    diffs([Big|Tail], [Big - Small | Diffs]).

count(Elements) ->
    count(Elements, #{}).

count([], Counter) -> maps:to_list(Counter);
count([H|T], Counter) ->
    N = maps:get(H, Counter, 0),
    count(T, Counter#{ H => N + 1 }).
