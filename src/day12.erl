-module(day12).

-export([solve_part1/1, solve_part2/1]).

% for tests
-export([parse/1]).

%%% solution

solve_part1(_Input) ->
    undefined.

solve_part2(_Input) ->
    undefined.

%%% parse

parse(Instructions) ->
    [{instruction_to_atom(H), list_to_integer(T)} ||
     [H|T] <- string:lexemes(Instructions, "\n")].

instruction_to_atom(Char) ->
    case Char of
        $N -> n;
        $S -> s;
        $E -> e;
        $W -> w;
        $L -> l;
        $R -> r;
        $F -> f
    end.
