-module(day12).

-export([solve_part1/1, solve_part2/1]).

% for tests
-export([parse/1, cruise/3, turn/2]).

%%% solution

solve_part1(Input) ->
    {{Lon, Lat}, _Heading} = cruise({0, 0}, 0, parse(Input)),
    abs(Lon) + abs(Lat).

solve_part2(_Input) ->
    undefined.

%%% Parsing

parse(Instructions) ->
    [{instruction_to_atom(H), list_to_integer(T)} ||
     [H|T] <- string:lexemes(Instructions, "\n")].

instruction_to_atom(Char) ->
    case Char of
        $N -> north;
        $S -> south;
        $E -> east;
        $W -> west;
        $L -> left;
        $R -> right;
        $F -> forward
    end.

%%% Movement

cruise(InitialPosition, InitialHeading, Instructions) ->
    lists:foldl(fun(Instruction, {Position, Heading}) ->
                        advance(Position, Heading, Instruction) end,
                        {InitialPosition, InitialHeading},
                        Instructions).

% turn
advance(Position, Heading, {left, Degrees}) ->
    {Position, turn(Heading, Degrees)};
advance(Position, Heading, {right, Degrees}) ->
    {Position, turn(Heading, -Degrees)};
% absolute shift
advance({East, North}, Heading, {north, Units}) ->
    {{East, North + Units}, Heading};
advance({East, North}, Heading, {south, Units}) ->
    {{East, North - Units}, Heading};
advance({East, North}, Heading, {east, Units}) ->
    {{East + Units, North}, Heading};
advance({East, North}, Heading, {west, Units}) ->
    {{East - Units, North}, Heading};
% relative shift
advance(Position, Heading, {forward, Units}) ->
    advance(Position, Heading, {heading_to_direction(Heading), Units}).

%% @doc Turns a given heading by Angle degrees, returns a value in [0, 360].
turn(Heading, Angle) ->
    (((Heading + Angle) rem 360) + 360) rem 360.

heading_to_direction(Heading) ->
    case Heading of
        0 -> east;
        90 -> north;
        180 -> west;
        270 -> south
    end.