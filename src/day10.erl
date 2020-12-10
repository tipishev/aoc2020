-module(day10).

-export([solve_part1/1, solve_part2/1]).

-export([jolt_diff_distro/1]).

%%% solution

solve_part1(BagAdapters) ->
    [{1, A}, {3, B}] = jolt_diff_distro(BagAdapters),
    A * B.

solve_part2(BagAdapters) ->
    count_arrangemets(BagAdapters).

% Part1

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

% sweet little counter <3
count([], Counter) -> maps:to_list(Counter);
count([H|T], Counter) ->
    N = maps:get(H, Counter, 0),
    count(T, Counter#{ H => N + 1 }).

% Part2

count_arrangements(BagAdapters) ->
    Outlet = 0,
    Device = lists:max(BagAdapters) + 3,
    AllVertices = lists:sort([Outlet, Device | BagAdapters]),
    Digraph = build_digraph(AllVertices),
    path_count(Outlet, Device).
