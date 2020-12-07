-module(day7).

-export([solve_part1/1, solve_part2/1]).

-compile([export_all]).  % FIXME

%%% solution

solve_part1(Input) ->
    parse(Input).

solve_part2(_Input) ->
    undefined.

%%% internals

%%% Parse

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
    lists:sort(Content);
parse_content([QuantityStr|ColorAndTail], Content) ->
    Quantity = list_to_integer(QuantityStr),
    {Color, Tail} = parse_color("", ColorAndTail),
    parse_content(Tail, [{Color, Quantity}|Content]).

%%% Graph Building

build_graph(BagLines) ->
    Bags = digraph:new([acyclic]),
    ColorRegistry = #{},
    build_graph(Bags, BagLines, ColorRegistry).


build_graph(Bags, [], _ColorRegistry) ->
    Bags;
build_graph(Bags, [{Color, Content} | Tail], ColorRegistry) ->
    {ParentVertex, NewColorRegistry} = get_vertex(Bags, Color, ColorRegistry),
    [].
    % % first line
    % LightRed = add_vertex(Bags, "light red"),
    % BrightWhite = add_vertex(Bags, "bright white"),
    % MutedYellow = add_vertex(Bags, "muted yellow"),
    % add_edge(Bags, LightRed, BrightWhite, 1),
    % add_edge(Bags, LightRed, MutedYellow, 2),

    % % second line
    % DarkOrange = add_vertex(Bags, "dark orange"),
    % add_edge(Bags, DarkOrange, BrightWhite, 3),
    % add_edge(Bags, DarkOrange, MutedYellow, 4),
    % Bags.

% TODO deuglify with {or, [pattern matching, sugar, multiple heads]}
get_vertex(Bags, ColorRegistry, Color) ->
    case maps:is_key(Color, ColorRegistry) of
        true ->
           ExistingVertex = maps:get(Color, ColorRegistry),
           {ExistingVertex, ColorRegistry};
        false ->
            NewVertex = add_vertex(Bags, Color),
            NewColorRegistry = ColorRegistry#{Color=>NewVertex},
            {NewVertex, NewColorRegistry}
    end.


add_vertex(Digraph, Label) ->
    digraph:add_vertex(Digraph, digraph:add_vertex(Digraph), Label).

add_edge(Digraph, ParentVertex, ChildVertex, Label) ->
    digraph:add_edge(Digraph, ParentVertex, ChildVertex, Label).

