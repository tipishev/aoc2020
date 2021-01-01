-module(day15).

-export([solve_part1/1, solve_part2/1]).

-export([parse/1, spoken/2]).

%%% solution

solve_part1(_Input) ->
    undefined.

solve_part2(_Input) ->
    undefined.

%%% Parse

parse(Input) ->
    [list_to_integer(N) || N <- string:lexemes(Input, ",")].

%%% Solve Part 1

spoken(Starting, Turn) ->
    spoken(Starting, Turn, _Mrsn=undefined, _Mem=#{}).

%% @doc Mrsn is Most Recently Spoken Number
spoken(Starting, Turn, _Mrsn, _Mem)
  when Turn =< length(Starting)->
    lists:nth(Turn, Starting);

spoken(_Starting, _Turn, Mrsn, Mem) ->
    Spoken = case maps:is_key(Mrsn, Mem) of
        true -> todo;
        false -> 0
    end,
    Spoken.
