-module(day12).

-export([solve_part1/1, solve_part2/1]).

% for tests
-export([
         parse/1,
         cruise/3, wp_cruise/3,
         turn/2, rotate/2
        ]).

%%% solution

solve_part1(Input) ->
    {{Lon, Lat}, _Heading} = cruise({0, 0}, 0, parse(Input)),
    abs(Lon) + abs(Lat).

solve_part2(Input) ->
    {{Lon, Lat}, {DLon, DLat}} = wp_cruise({0, 0}, {10, 1}, parse(Input)),
    abs(Lon) + abs(Lat).

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

%% @doc Cruise using instructions for the ship (part 1).
cruise(InitialPosition, InitialHeading, Instructions) ->
    lists:foldl(fun(Instruction, {Position, Heading}) ->
                        advance(Position, Heading, Instruction) end,
                        {InitialPosition, InitialHeading},
                        Instructions).

%% @doc Cruise using instructions for the waypoing (part 2).
wp_cruise(InitialPosition, InitialWaypointRelativePosition, Instructions) ->
    lists:foldl(fun(Instruction, {Position, WaypointRelativePosition}) ->
                        wp_advance(Position, WaypointRelativePosition,
                                   Instruction) end,
                        {InitialPosition, InitialWaypointRelativePosition},
                        Instructions).

%% @doc Advance the ship position according to a single instruction.
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

%% @doc Advance the ship/waypoint position according to a single instruction.
% turn waypoint
wp_advance(Position, WaypointRelativePosition, {left, Degrees}) ->
    {Position, rotate(WaypointRelativePosition, Degrees)};
wp_advance(Position, WaypointRelativePosition, {right, Degrees}) ->
    {Position, rotate(WaypointRelativePosition, -Degrees)};

%%% Herlpers

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

%% @doc rotate vector {X, Y} around {0, 0} by Angle degrees.
rotate({X, Y}, Angle) ->
    RadAngle = to_radians(Angle),
    Sin = math:sin(RadAngle),
    Cos = math:cos(RadAngle),
    {round(X * Cos - Y * Sin), round(X * Sin + Y * Cos)}.


to_radians(Degrees) ->
    (Degrees / 180) * math:pi().
