-module(day10).

-export([solve_part1/1, solve_part2/1]).

-export([jolt_diff_distro/1]).

%%% solution

solve_part1(BagAdapters) ->
    [{1, OnesCount},
     {3, ThreesCount}] = jolt_diff_distro(BagAdapters),
    OnesCount * ThreesCount.

solve_part2(BagAdapters) ->
    Outlet = 0,
    Device = lists:max(BagAdapters) + 3, 
    Joltages = [Outlet, Device | BagAdapters],
    _Digraph = build_digraph(Joltages).

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

% adds vertex for each number, creates edge A->B
% if (B - A) <= 3
build_digraph(Joltages) ->
    Digraph = digraph:new([acyclic]),
    VertexRegistry = add_vertices(Digraph, _Labels=Joltages),
    add_edges(Digraph, VertexRegistry).

%% Adds vertices to the digraph and returns {label:vertex} registry 
add_vertices(Digraph, Labels) ->
    lists:foldl(
      fun(Label, VertexRegistry) ->
          VertexRegistry#{Label => add_vertex(Digraph, Label)}
      end, #{}, Labels).

%% Adds a labelled vertex to Digraph
add_vertex(Digraph, Label) ->
    Vertex = digraph:add_vertex(Digraph),
    digraph:add_vertex(Digraph, Vertex, Label).

%% connects vertices whose values differ no more than by 3
add_edges(Digraph, VertexRegistry) ->
    {Labels, _} = lists:unzip(maps:to_list(VertexRegistry)),
    Edges = generate_edges(Labels, []),
    _ = [digraph:add_edge( Digraph,
                           maps:get(V1, VertexRegistry), 
                           maps:get(V2, VertexRegistry))
         || {V1, V2} <- Edges],
    Digraph.

% final check
generate_edges([Suitor, Sink], Edges) ->
    lists:flatten([check(Suitor, Sink) | Edges]);
% pre-final check
generate_edges([Suitor, A, Sink], Edges) ->
    NewEdges = [check(Suitor, A), check(Suitor, Sink) | Edges],
    generate_edges([A, Sink], NewEdges); 
% common case, at least 4 elements in list
generate_edges([Suitor, A, B, C | Tail], Edges) ->
    NewEdges = [check(Suitor, A),
                check(Suitor, B),
                check(Suitor, C) | Edges],
    generate_edges([A, B, C | Tail], NewEdges).

check(Small, Big) when (Big - Small) =< 3 -> {Small, Big};
check(_, _) -> [].  % flatten will remove these

