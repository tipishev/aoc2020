-module(day7).

-export([solve_part1/1, solve_part2/1]).

-compile([export_all]).  % FIXME

%%% solution

solve_part1(Input) ->
    Rules = parse(Input),
    {Vertices, Edges} = extract_vertices_and_edges(Rules),
    {Bags, ColorRegistry} = build_digraph(Vertices, Edges),
    ShinyGoldVertex = maps:get("shiny gold", ColorRegistry),
    UpstreamWithItself = get_upstream_labels(Bags, ShinyGoldVertex),
    UpstreamWithoutItself = lists:delete("shiny gold", UpstreamWithItself),
    length(UpstreamWithoutItself).

solve_part2(Input) ->
    Rules = parse(Input),
    {Vertices, Edges} = extract_vertices_and_edges(Rules),
    {Bags, ColorRegistry} = build_digraph(Vertices, Edges),
    ShinyGoldVertex = maps:get("shiny gold", ColorRegistry),
    sum_downstream_edges(Bags, ShinyGoldVertex) - 1.

%%% internals

%%% Input Parsing

parse(Input) ->
    Lines = string:lexemes(Input, "\n"),
    lists:map(fun parse_line/1, Lines).

parse_line(Line) ->
    Words = string:lexemes(Line, "., "),
    {Color, ["contain" | ContentStr]} = parse_color("", Words),
    Content = parse_content(ContentStr),
    {Color, Content}.

parse_color(Color, ["bag" | Tail]) ->
    {Color, Tail};
parse_color(Color, ["bags" | Tail]) ->
    {Color, Tail};
parse_color(ColorSoFar, [ColorWord|OtherWords]) ->
    NewColorSoFar = case ColorSoFar =:= "" of
                        true -> ColorWord;
                        false -> ColorSoFar ++ " " ++ ColorWord
                    end,
    parse_color(NewColorSoFar, OtherWords).

parse_content(ContentStr) ->
    parse_content(ContentStr, _Content=[]).

parse_content(["no"|_RestOfLine], _Content) ->
    [];
parse_content([], Content) ->
    lists:sort(Content);  % TODO adjust tests to avoid sorting
parse_content([QuantityStr|ColorAndTail], Content) ->
    Quantity = list_to_integer(QuantityStr),
    {Color, Tail} = parse_color("", ColorAndTail),
    parse_content(Tail, [{Color, Quantity}|Content]).

%%% Graph Building

extract_vertices_and_edges(Rules) ->
    Vertices = sets:new(),
    Edges = [],
    extract_vertices_and_edges(Rules, Vertices, Edges).

extract_vertices_and_edges([], Vertices, Edges) ->
    {sets:to_list(Vertices), Edges};
extract_vertices_and_edges([Rule | OtherRules], Vertices, Edges) ->
    {Color, Content} = Rule,
    AdditionalEdges = generate_edges(Color, Content, _EdgesAcc=[]),
    NewVertices = sets:add_element(Color, Vertices),
    NewEdges = Edges ++ AdditionalEdges,   % TODO inefficient
    extract_vertices_and_edges(OtherRules, NewVertices, NewEdges).

generate_edges(_Color, [], EdgesAcc) ->
    EdgesAcc;
generate_edges(Color, [{AnotherColor, Quantity} | Tail], EdgesAcc) ->
    generate_edges(Color, Tail, [{Color, AnotherColor, Quantity} | EdgesAcc]).


build_digraph(Vertices, Edges) ->
    % TODO combine Bags and ColorRegistry in a record?
    Bags = digraph:new([acyclic]),  % mutable AF

    % register all vertices
    ColorRegistry = lists:foldl(
                      fun(Color, Registry) ->
                              add_vertex(Bags, Registry, Color)
                      end, #{}, Vertices),

    % create all the edges
    _ = [add_edge(Bags, ColorRegistry, ParentLabel, ChildLabel, Quantity)
         || {ParentLabel, ChildLabel, Quantity} <- Edges],
    {Bags, ColorRegistry}.


% TODO deuglify with {$or, [pattern matching, sugar, multiple heads]}
add_vertex(Digraph, VertexRegistry, Label) ->
    Vertex = digraph:add_vertex(Digraph, digraph:add_vertex(Digraph), Label),
    VertexRegistry#{Label=>Vertex}.


add_edge(Digraph, VertexRegistry, ParentLabel, ChildLabel, Label) ->
    ParentVertex = maps:get(ParentLabel, VertexRegistry),
    ChildVertex = maps:get(ChildLabel, VertexRegistry),
    digraph:add_edge(Digraph, ParentVertex, ChildVertex, Label).

%%% Graph Operations

get_upstream_labels(Digraph, Vertex) ->
    OwnLabel = extract_label(Digraph, Vertex),
    % io:format("~p~n", [OwnLabel]),
    InNeighbours = digraph:in_neighbours(Digraph, Vertex),
    ListOfLists = [[OwnLabel] | [get_upstream_labels(Digraph, InNeighbour)
                   || InNeighbour <- InNeighbours]],
    WithDupes = lists:merge(ListOfLists),
    dedupe(WithDupes).

sum_downstream_edges(Digraph, Vertex) ->
    OutEdges = digraph:out_edges(Digraph, Vertex),
    case OutEdges =:= [] of
        true ->
            1;
        false ->
            Weights = lists:map(fun(OutEdge) ->
                                    edge_total_weight(Digraph, OutEdge)
                                end,
                                OutEdges),
            1 + lists:sum(Weights)
    end.

edge_total_weight(Digraph, Edge) ->
    {Weight, Vertex} = extract_weight_and_vertex(Digraph, Edge),
    Weight * sum_downstream_edges(Digraph, Vertex).


%%% sillly utils
dedupe([])    -> [];
dedupe([H|T]) -> [H | [X || X <- dedupe(T), X /= H]].

extract_label(Digraph, Vertex) ->
    {Vertex, Label} = digraph:vertex(Digraph, Vertex),
    Label.

extract_weight_and_vertex(Digraph, Edge) ->
    {Edge, _VertexFrom, VertexTo, Weight} = digraph:edge(Digraph, Edge),
    {Weight, VertexTo}.

